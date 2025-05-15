package enfold

import "base:runtime"
import "core:fmt"
import "core:log"

Expr :: union {
	EBool,
	EInt,
	EString,
	EVar,
	EProp,
	EApp,
	EParen,
	EBlock,
}

// {{{ Simple expressions
EBool :: struct {
	tok:   Token,
	value: bool,
}

EInt :: struct {
	tok:   Token,
	value: i128,
}

EString :: struct {
	tok:   Token,
	value: string,
}

EVar :: struct {
	name: Token,
}

EProp :: struct {
	tok:  Token,
	name: string,
}

EApp :: struct {
	function: ^Expr,
	argument: ^Expr,
}

EParen :: struct {
	lparen: Token,
	expr:   ^Expr,
	rparen: Token,
}
// }}}
// {{{ Statements
Block_Kind :: enum {
	Effect,
	List,
	Object,
	Multi,
}

Block_Args :: struct {
	lparen: Token,
	names:  []Token,
	rparen: Token,
}

ST_Declaration :: struct {
	vars:   []Token,
	walrus: Token,
	value:  Expr,
}

ST_Assignment :: struct {
	path_toks: []Token,
	path:      []string,
	eq:        Token,
	value:     Expr,
}

ST_Expr :: struct {
	expr: ^Expr,
}

ST_Globals :: struct {
	names: []Token,
}

Block_Statement :: union {
	ST_Declaration,
	ST_Assignment,
	ST_Expr,
	ST_Globals,
}

EBlock :: struct {
	kind_tok: Token `fmt:"-"`,
	kind:     Block_Kind,
	// Null when this is not a function
	args:     Block_Args,
	no_align: bool `fmt:"-"`,
	contents: []Block_Statement,
}
// }}}

// {{{ Parsing state
Indentation :: u8
Indentation_Relation :: enum {
	Eq,
	Gt,
	Gte,
	Any,
}

Indentation_Range :: [2]Indentation

Parser :: struct {
	lexer:           Lexer,
	curr:            Token,
	alloc:           runtime.Allocator,
	labels:          ^[dynamic]string, // Keeps track of what we are parsing

	// Indentation state
	indent_range:    Indentation_Range,
	indent_relation: Indentation_Relation,
	indent_absolute: bool,
}

Parser_Error :: struct {
	loc:   Source_Loc,
	msg:   string,
	stack: []string,
}

// Returned when a parser refuses to continue along a branch, but before
// committing to said branch, letting the parent choose another one
Parser_Cancellation :: struct {
}

parser_error :: proc(parser: ^Parser, msg: string, loc: Source_Loc = {}) -> Parser_Error {
	return Parser_Error {
		msg = msg,
		stack = parser.labels[:],
		loc = loc == {} ? parser.curr.from : loc,
	}
}

mk_parser :: proc(
	source: string,
	alloc: runtime.Allocator,
) -> (
	parser: Parser,
	err: Enfold_Error,
) {
	lexer := mk_lexer(source) or_return
	parser = Parser {
		lexer           = lexer,
		indent_relation = .Gte,
		indent_range    = {1, ~Indentation(0)},
		indent_absolute = true,
		alloc           = alloc,
	}

	parser.labels = new_clone(make([dynamic]string, 0, 64, alloc), alloc)

	next_token(&parser) or_return

	return parser, nil
}


@(private = "file")
@(deferred_in = post_label)
label :: proc(parser: ^Parser, label: string) {
	append(parser.labels, label)
}

@(private = "file")
post_label :: proc(parser: ^Parser, label: string) {
	pop(parser.labels)
}
// }}}

// {{{ Indent state helpers
@(private = "file")
@(deferred_in = unabsolute)
absolute :: proc(parser: ^Parser) {
	parser.indent_absolute = true
}

@(private = "file")
unabsolute :: proc(parser: ^Parser) {
	parser.indent_absolute = false
}

@(private = "file")
@(deferred_in_out = post_with_relation)
with_relation :: proc(
	parser: ^Parser,
	relation: Indentation_Relation,
) -> (
	pre_relation: Indentation_Relation,
	pre_range: Indentation_Range,
) {
	if parser.indent_absolute {return}

	initial_range := parser.indent_range
	initial_relation := parser.indent_relation

	top := ~Indentation(0) // max indentation

	// Applies the relation
	switch relation {
	case .Eq:
		break // Is this even correct? Is EQ even used???
	case .Gte:
		parser.indent_range = {parser.indent_range[0], top}
	case .Gt:
		parser.indent_range = {parser.indent_range[0] + 1, top}
	case .Any:
		parser.indent_range = {1, top}
	}

	parser.indent_relation = relation

	return initial_relation, initial_range
}

@(private = "file")
post_with_relation :: proc(
	parser: ^Parser,
	relation: Indentation_Relation,
	pre_relation: Indentation_Relation,
	pre_range: Indentation_Range,
) {
	// In this case, the parser was in absolute mode when we did the initial call
	if pre_relation == {} {return}

	intersect_ranges :: proc(a, b: Indentation_Range) -> Indentation_Range {
		return {max(a.x, b.x), min(a.y, b.y)}
	}

	// Un-applies the relation
	switch relation {
	case .Eq:
		parser.indent_range = intersect_ranges(pre_range, parser.indent_range)
	case .Gte:
		parser.indent_range = intersect_ranges(pre_range, {1, parser.indent_range.y})
	case .Gt:
		parser.indent_range = intersect_ranges(pre_range, {1, parser.indent_range.y - 1})
	case .Any:
		parser.indent_range = pre_range
	}

	parser.indent_relation = pre_relation
}
// }}}
// {{{ Token peeking/advancement
@(require_results)
next_token :: proc(parser: ^Parser, relation: Indentation_Relation = .Gt) -> (err: Enfold_Error) {
	with_relation(parser, relation)

	if parser.curr.content != {} {
		c := Indentation(parser.curr.from.col)

		if c < parser.indent_range[0] || c > parser.indent_range[1] {
			context.temp_allocator = parser.alloc
			return parser_error(
				parser,
				fmt.tprintf(
					"invalid indentation (not in range %v..%v; absolute: %v)",
					parser.indent_range[0],
					parser.indent_range[1],
					parser.indent_absolute,
				),
			)
		}

		// Collapse indentation range to that of the token
		parser.indent_absolute = false
		parser.indent_range = {c, c}
	}

	for {
		tok := tokenize(&parser.lexer) or_return

		#partial switch tok.kind {
		case .Newline, .Comment:
			continue
		case:
			parser.curr = tok
			return
		}
	}
}

peek :: proc(
	parser: ^Parser,
	relation: Indentation_Relation = .Gt,
) -> (
	tok: Token,
	err: Enfold_Error,
) {
	with_relation(parser, relation)
	c := Indentation(parser.curr.from.col)

	if c < parser.indent_range[0] || c > parser.indent_range[1] {
		return {}, Parser_Cancellation{}
	}

	return parser.curr, nil
}
// }}}

// {{{ Assignments
@(private = "file")
parse_assignment :: proc(parser: ^Parser) -> (stmt: ST_Assignment, err: Enfold_Error) {
	label(parser, "assignment")

	if tok := peek(parser) or_return; tok.kind != .Identifier {
		return stmt, Parser_Cancellation{}
	}

	path_toks := make([dynamic]Token, 0, 2, parser.alloc)
	append(&path_toks, parser.curr)
	next_token(parser) or_return

	for {
		tok := peek(parser) or_break
		(tok.kind == .Property) or_break

		append(&path_toks, parser.curr)
		next_token(parser) or_return
	}

	stmt.path_toks = path_toks[:]

	if parser.curr.kind != .Equal {
		return stmt, Parser_Cancellation{}
	}

	stmt.eq = parser.curr
	next_token(parser) or_return

	// We're commited now, so let's create a list of strings for the path
	stmt.path = make_slice([]string, len(path_toks), parser.alloc)
	for s, i in path_toks {
		if i == 0 {
			stmt.path[i] = stmt.path_toks[i].content
		} else {
			stmt.path[i] = string(stmt.path_toks[i].content[1:])
		}
	}

	stmt.value = parse_expr(parser) or_return

	return stmt, nil
}
// }}}
// {{{ Declarations
@(private = "file")
parse_declaration :: proc(parser: ^Parser) -> (stmt: ST_Declaration, err: Enfold_Error) {
	label(parser, "declaration")

	if tok := peek(parser) or_return; tok.kind != .Identifier {
		return stmt, Parser_Cancellation{}
	}

	vars := make([dynamic]Token, 0, 2, parser.alloc)
	append(&vars, parser.curr)
	next_token(parser) or_return

	for {
		tok := peek(parser) or_break
		(tok.kind == .Comma) or_break

		next_token(parser) or_return

		if parser.curr.kind != .Identifier {
			return stmt, parser_error(parser, "expected identifier")
		} else {
			append(&vars, parser.curr)
			next_token(parser) or_return
		}
	}

	stmt.vars = vars[:]

	if parser.curr.kind != .Walrus {
		if len(vars) == 1 {
			return stmt, Parser_Cancellation{}
		} else {
			return stmt, parser_error(parser, "expected :=")
		}
	}

	stmt.walrus = parser.curr
	next_token(parser) or_return

	stmt.value = parse_expr(parser) or_return

	return stmt, nil
}
// }}}
// {{{ Globals
@(private = "file")
parse_globals :: proc(parser: ^Parser) -> (stmt: ST_Globals, err: Enfold_Error) {
	label(parser, "globals")

	if tok := peek(parser) or_return; tok.kind != .Global {
		return stmt, Parser_Cancellation{}
	}

	next_token(parser) or_return

	names := make([dynamic]Token, 0, 2, parser.alloc)

	for {
		tok := peek(parser) or_break

		if tok.kind == .Comma {
			next_token(parser) or_return
			continue
		}

		if tok.kind != .Identifier {
			return stmt, parser_error(parser, "expected identifier")
		}

		append(&names, parser.curr)
		next_token(parser) or_return
	}

	stmt.names = names[:]

	return stmt, nil
}
// }}}
// {{{ Statements
@(private = "file")
parse_statement :: proc(parser: ^Parser) -> (stmt: Block_Statement, err: Enfold_Error) {
	label(parser, "statement")
	og_parser: Parser = parser^ // Save the parser state

	stmt, err = parse_assignment(parser)
	if parser_cancelled(err) {
		parser^ = og_parser
	} else {
		return stmt, err
	}

	stmt, err = parse_declaration(parser)
	if parser_cancelled(err) {
		parser^ = og_parser
	} else {
		return stmt, err
	}

	stmt, err = parse_globals(parser)
	if parser_cancelled(err) {
		parser^ = og_parser
	} else {
		return stmt, err
	}

	expr := try_parse_expr(parser) or_return

	stmt = ST_Expr {
		expr = new_clone(expr, parser.alloc),
	}

	return stmt, nil
}
// }}}
// {{{ Applications
parse_toplevel_expr :: proc(parser: ^Parser) -> (expr: Expr, err: Enfold_Error) {
	expr = parse_expr(parser) or_return

	if parser.curr.kind != .Eof {
		err = parser_error(parser, "expected eof")
	}

	return
}

parse_expr :: proc(parser: ^Parser) -> (expr: Expr, err: Enfold_Error) {
	expr, err = try_parse_expr(parser)

	if parser_cancelled(err) {
		err = parser_error(parser, "expected expression")
	}

	return
}

try_parse_expr :: proc(parser: ^Parser) -> (expr: Expr, err: Enfold_Error) {
	label(parser, "expression")
	result := parse_single_expr(parser) or_return

	for {
		arg, err := parse_single_expr(parser)

		(!parser_cancelled(err)) or_break
		err or_return

		result = EApp {
			function = new_clone(result, parser.alloc),
			argument = new_clone(arg, parser.alloc),
		}
	}

	return result, nil
}
// }}}

parse_single_expr :: proc(parser: ^Parser) -> (expr: Expr, err: Enfold_Error) {
	tok := peek(parser) or_return
	#partial switch tok.kind {
	// {{{ Bools
	case .Bool:
		res: bool = parser.curr.content == "true"
		expr = EBool {
			tok   = parser.curr,
			value = res,
		}

		next_token(parser) or_return
	// }}}
	// {{{ Integers
	case .Integer:
		res: i128

		for c in parser.curr.content {
			res = res * 10 + i128(c - '0')
		}

		expr = EInt {
			tok   = parser.curr,
			value = res,
		}

		next_token(parser) or_return
	// }}}
	// {{{ Strings
	case .String:
		expr = EString {
			tok   = parser.curr,
			value = parser.curr.content[1:len(parser.curr.content) - 1],
		}

		next_token(parser) or_return
	// }}}
	// {{{ Vars
	case .Identifier:
		expr = EVar {
			name = parser.curr,
		}

		next_token(parser) or_return
	case .Property:
		expr = EProp {
			tok  = parser.curr,
			name = string(parser.curr.content[1:]),
		}

		next_token(parser) or_return
	// }}}
	// {{{ ( expr )
	case .LParen:
		lparen := parser.curr
		next_token(parser) or_return

		inner, err := parse_expr(parser)

		rparen := parser.curr
		if rparen.kind != .RParen {return expr, parser_error(parser, "expected )")}
		next_token(parser) or_return

		expr = EParen {
			lparen = lparen,
			rparen = rparen,
			expr   = new_clone(inner, parser.alloc),
		}
	// }}}
	// {{{ Blocks
	case .Multi, .Effect, .List, .Object:
		block: EBlock

		// {{{ Kind
		{
			label(parser, "block kind")
			#partial switch parser.curr.kind {
			case .Multi:
				block.kind = .Multi
			case .Effect:
				block.kind = .Effect
			case .List:
				block.kind = .List
			case .Object:
				block.kind = .Object
			}

			block.kind_tok = parser.curr
			next_token(parser) or_return
		}
		// }}}
		// {{{ Args
		if parser.curr.kind == .LParen {
			block.args.lparen = parser.curr
			next_token(parser) or_return

			// Start with capacity 4, because why not
			args := make([dynamic]Token, 0, 4, parser.alloc)

			for {
				if parser.curr.kind == .RParen {break}
				if parser.curr.kind != .Identifier {
					return expr, parser_error(parser, "expected argument name")
				}

				append(&args, parser.curr)
				next_token(parser) or_return

				// Optional commas
				if parser.curr.kind == .Comma {
					next_token(parser) or_return
				}
			}

			block.args.names = args[:]

			block.args.rparen = parser.curr
			if parser.curr.kind != .RParen {return expr, parser_error(parser, "expected )")}

			// We pass .Gte in order to allow closing the parenthesis on the left-most
			// allowed line
			next_token(parser, .Gte) or_return
		}
		// }}}
		// {{{ Modifiers
		for parser.curr.kind == .No_Align {
			block.no_align = true

			if parser.curr.from.line != block.kind_tok.from.line {
				return expr, parser_error(parser, "block modifiers cannot go on a separate line")
			}

			next_token(parser) or_return
		}
		// }}}

		statements := make([dynamic]Block_Statement, 0, 16, parser.alloc)

		if !block.no_align && block.kind_tok.from.line == parser.curr.from.line {
			// {{{ Inline statements
			for {
				statement, err := parse_statement(parser)

				(!parser_cancelled(err)) or_break
				err or_return

				append(&statements, statement)

				tok := peek(parser) or_break
				(tok.kind == .Comma) or_break
				next_token(parser) or_return
			}
			// }}}
		} else {
			with_relation(parser, .Gt)
			// {{{ Multiline blocks
			for {
				absolute(parser)
				statement, err := parse_statement(parser)

				(!parser_cancelled(err)) or_break
				err or_return

				append(&statements, statement)
			}
			// }}}
		}

		block.contents = statements[:]
		expr = block
	// }}}
	case:
		return expr, Parser_Cancellation{}
	}

	return expr, nil
}

parse_toplevel_block :: proc(parser: ^Parser) -> (block: EBlock, err: Enfold_Error) {
	block.kind = .Effect
	statements := make([dynamic]Block_Statement, 0, 16, parser.alloc)

	for {
		absolute(parser)
		statement, err := parse_statement(parser)

		(!parser_cancelled(err)) or_break
		err or_return

		append(&statements, statement)
	}

	block.contents = statements[:]

	if parser.curr.kind != .Eof {
		err = parser_error(parser, "expected eof")
	}

	return
}
