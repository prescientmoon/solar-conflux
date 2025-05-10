package enfold

import "core:log"
import "core:math"

Expr :: union {
	EInt,
	EString,
	EApp,
	EParen,
	EBlock,
}

// {{{ Simple expressions
EInt :: struct {
	tok:   Token,
	value: i128,
}

EString :: struct {
	tok:   Token,
	value: string,
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

Block_Modifier :: struct {
	tok: Token,
}

ST_Declaration :: struct {
	vars:   []Token,
	walrus: Token,
	value:  Token,
}

ST_Assignment :: struct {
	name:  Token,
	eq:    Token,
	value: Token,
}

ST_Expr :: struct {
	expr: ^Expr,
}

Block_Statement :: union {
	ST_Declaration,
	ST_Assignment,
	ST_Expr,
}

EBlock :: struct {
	kind:      Block_Kind,
	// Null when this is not a function
	args:      ^Block_Args,
	modifiers: []Block_Modifier,
	contents:  []Block_Statement,
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
	source:          string,
	lexer:           Lexer,
	curr:            Token,

	// Indentation state
	indent_range:    Indentation_Range,
	indent_relation: Indentation_Relation,
	indent_absolute: bool,
}

@(private = "file")
fail :: proc(parser: ^Parser, pos: Source_Loc, msg: string) {
	log.panicf("Error at %v: %v", pos, msg)
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
	switch parser.indent_relation {
	case .Eq:
		break
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
	switch parser.indent_relation {
	case .Eq:
		parser.indent_range = intersect_ranges(pre_range, parser.indent_range)
	case .Gte:
		parser.indent_range = intersect_ranges(pre_range, {0, parser.indent_range.y})
	case .Gt:
		parser.indent_range = intersect_ranges(pre_range, {0, parser.indent_range.y - 1})
	case .Any:
		parser.indent_range = pre_range
	}

	parser.indent_relation = pre_relation
}
// }}}
// {{{ Token peeking/advancement 
next_token :: proc(parser: ^Parser) {
	c := Indentation(parser.curr.from.col)

	if c < parser.indent_range[0] || c > parser.indent_range[1] {
		fail(parser, parser.curr.from, "invalid indentation")
	}

	// Collapse indentation range to that of the token
	parser.indent_absolute = false
	parser.indent_range = {c, c}

	parser.curr = tokenize(&parser.lexer)
}

peek :: proc(parser: ^Parser) -> (tok: Token, ok: bool) {
	c := Indentation(parser.curr.from.col)

	if c < parser.indent_range[0] || c > parser.indent_range[1] {
		return tok, false
	}

	return tok, true
}
// }}}
