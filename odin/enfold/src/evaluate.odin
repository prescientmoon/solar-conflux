package enfold

import "base:runtime"
import "core:fmt"
import "core:log"

// {{{ Normalized expressions
// Normalized expressions are a desugared version of the AST used 
// for codegen.
//
// Changes from the base CST:
// - lists, objects, applications, etc do not contain effects
// - arbitrary blocks cannot have arguments
// - multi blocks get inlined into function applications
// - lists, objects, and effects all hold specific data 
//   instead of containing general statements
// - effects only get to exist inside lambdas
// - property access is not its own expression instead of function
//   application syntax sugar
// - variables are always bound before appearing in some scope
// - objects no longer contain nested paths
NExpr :: union {
	EBool,
	EInt,
	EString,
	NVar,
	NApp,
	NProp_Access,
	NObject,
	NList,
	NLambda,
}

NVar :: struct {
	tok:  Token,
	name: string,
}


NApp :: struct {
	function:  ^NExpr,
	arguments: []NExpr,
}

NProp_Access :: struct {
	object:   ^NExpr,
	prop:     string,
	prop_tok: Token,
}

NList :: struct {
	elements: []NExpr,
}

NObject_Entry :: struct {
	name_tok: Token,
	name:     string,
	value:    NExpr,
}

NObject :: struct {
	// NOTE: the indirection is here so 
	// we can edit the values of the object 
	// after the object gets created
	elements: ^[dynamic]NObject_Entry,
}

NST_Declaration :: struct {
	vars:  []Token,
	value: NExpr,
}

NStatement :: union {
	NST_Declaration,
	NApp,
}

NEffect :: struct {
	contents:     []NStatement,

	// nil when the block doesn't return
	block_return: NExpr,
}

NLambda :: struct {
	args: []Token,
	body: ^NEffect,
}
// }}}

@(private = "file")
Scope_Stack_Entry :: struct {
	prev_rename: string,
	source_name: Token,
	name:        string,
}


Evaluator :: struct {
	alloc:          runtime.Allocator,
	scope:          map[string]string,
	scope_stack:    [dynamic]Scope_Stack_Entry,
	current_effect: [dynamic]NStatement,
	rename_counter: uint,
}

Evaluator_Error :: struct {
	loc: Source_Loc,
	msg: string,
}

mk_evaluator :: proc(alloc: runtime.Allocator) -> (evaluator: Evaluator) {
	evaluator.alloc = alloc
	evaluator.scope = make(map[string]string, 32, alloc)
	evaluator.scope_stack = make([dynamic]Scope_Stack_Entry, 32, alloc)
	return
}

// {{{ Unwrap parenthesis
// Unwraps parenthesis around some expression repeatedly
@(private = "file")
expr_unparen :: proc(expr: Expr) -> Expr {
	expr := expr
	for {
		if inner, ok := expr.(EParen); ok {
			expr = inner.expr^
		} else {
			break
		}
	}

	return expr
}
// }}}
// {{{ Expr to source location
@(private = "file")
expr_pos :: proc(expr: Expr) -> Source_Loc {
	switch inner in expr {
	case EBool:
		return inner.tok.from
	case EInt:
		return inner.tok.from
	case EString:
		return inner.tok.from
	case EVar:
		return inner.name.from
	case EProp:
		return inner.tok.from
	case EApp:
		return expr_pos(inner.function^)
	case EParen:
		return expr_pos(inner.expr^)
	case EBlock:
		return inner.kind_tok.from
	}

	log.panicf("Unknown expression kind %v", expr)
}
// }}}
// {{{ Manipulate scopes
// Binds a new variable in the current scope. Returns the new variable name 
// created in order to avoid conflicts.
@(private = "file")
scope_push :: proc(evaluator: ^Evaluator, source_name: Token) -> string {
	rename := source_name.content
	prev_name: string

	if source_name.content in evaluator.scope {
		context.temp_allocator = evaluator.alloc
		rename = fmt.tprintf("%v__r%v", source_name.content, evaluator.rename_counter)
		evaluator.rename_counter += 1
	} else {
		prev_name = evaluator.scope[source_name.content]
	}

	entry := Scope_Stack_Entry {
		prev_rename = prev_name,
		name        = rename,
		source_name = source_name,
	}

	append(&evaluator.scope_stack, entry)
	evaluator.scope[source_name.content] = rename

	return rename
}

@(private = "file")
scope_pop :: proc(evaluator: ^Evaluator) {
	entry := pop(&evaluator.scope_stack)
	if entry.prev_rename != {} {
		evaluator.scope[entry.source_name.content] = entry.prev_rename
	} else {
		delete_key(&evaluator.scope, entry.source_name.content)
	}
}
// }}}

// {{{ Normalize effectful expression
expr_to_normalized_effect :: proc(
	evaluator: ^Evaluator,
	expr: Expr,
) -> (
	eff: NEffect,
	err: Enfold_Error,
) {
	prev_effect := evaluator.current_effect
	defer evaluator.current_effect = prev_effect
	evaluator.current_effect = make([dynamic]NStatement, 0, 8)

	expr := expr_to_normal(evaluator, expr) or_return
	effect := NEffect {
		contents     = evaluator.current_effect[:],
		block_return = expr,
	}

	return effect, nil
}
// }}}
// {{{ Store declarations inside the closest block
@(private = "file")
store_declaration :: proc(
	evaluator: ^Evaluator,
	decl: ST_Declaration,
	pop_counter: ^uint,
) -> (
	err: Enfold_Error,
) {
	value := expr_to_normal(evaluator, decl.value) or_return

	// TODO: support multi blocks
	if _, is_app := value.(NApp); value == nil || !is_app && len(decl.vars) > 1 {
		return Evaluator_Error {
			loc = decl.walrus.from,
			msg = "can only destructure function applications",
		}
	}

	var_names := make_slice([]Token, len(decl.vars), evaluator.alloc)
	copy_slice(var_names, var_names)

	for v, i in decl.vars {
		var_names[i].content = scope_push(evaluator, v)
	}

	append(&evaluator.current_effect, NST_Declaration{vars = var_names, value = value})

	pop_counter^ += len(decl.vars)

	return nil
}
// }}}
// {{{ Keep track of globals
@(private = "file")
store_globals :: proc(
	evaluator: ^Evaluator,
	decl: ST_Globals,
	pop_counter: ^uint,
) -> (
	err: Enfold_Error,
) {
	for v in decl.names {
		if v.content in evaluator.scope {
			return Evaluator_Error{loc = v.from, msg = "globals cannot shadow"}
		}

		scope_push(evaluator, v)
	}

	pop_counter^ += len(decl.names)

	return nil
}
// }}}
// {{{ Handle multi blocks
multi_block_to_norm :: proc(
	evaluator: ^Evaluator,
	block: EBlock,
) -> (
	out: []NExpr,
	err: Enfold_Error,
) {
	exprs := make([dynamic]NExpr, 0, 2, evaluator.alloc)

	pop_counter: uint = 0
	defer for _ in 0 ..< pop_counter {scope_pop(evaluator)}

	for st, i in block.contents {
		switch st_inner in st {
		case ST_Globals:
			store_globals(evaluator, st_inner, &pop_counter) or_return
		case ST_Declaration:
			store_declaration(evaluator, st_inner, &pop_counter) or_return
		case ST_Assignment:
			return nil, Evaluator_Error {
				loc = st_inner.eq.from,
				msg = "assignments are not permitted inside multi blocks",
			}
		case ST_Expr:
			norm := expr_to_normal(evaluator, st_inner.expr^) or_return
			if norm == nil {
				return nil, Evaluator_Error {
					loc = expr_pos(st_inner.expr^),
					msg = "invalid block inside multi block",
				}
			}

			append(&exprs, norm)
		}
	}

	return exprs[:], nil
}
// }}}

@(private = "file")
expr_to_normal :: proc(evaluator: ^Evaluator, expr: Expr) -> (out: NExpr, err: Enfold_Error) {
	switch inner in expr {
	case EBool:
		return NExpr(inner), nil
	case EInt:
		return NExpr(inner), nil
	case EString:
		return NExpr(inner), nil
	case EParen:
		return expr_to_normal(evaluator, inner.expr^)
	// {{{ Properties
	case EProp:
		// TODO: mark tokens as generated
		identifier := NVar {
			tok = Token{kind = .Identifier, content = "x"},
			name = "x",
		}

		prop_access := NProp_Access {
			prop     = inner.name,
			prop_tok = inner.tok,
			object   = new_clone(NExpr(identifier), evaluator.alloc),
		}

		body := NEffect {
			block_return = NExpr(prop_access),
		}

		args := make_slice([]Token, 1, evaluator.alloc)
		args[0] = identifier.tok
		lam := NLambda {
			args = args,
			body = new_clone(body, evaluator.alloc),
		}

		return NExpr(lam), nil
	// }}}
	// {{{ Variables
	case EVar:
		name := inner.name.content
		if !(name in evaluator.scope) {
			context.temp_allocator = evaluator.alloc
			return nil, Evaluator_Error {
				loc = inner.name.from,
				msg = fmt.tprintf("variable `%v` not in scope", name),
			}
			// return NExpr(NVar{tok = inner.name, name = name}), nil
		}

		return NExpr(NVar{tok = inner.name, name = evaluator.scope[name]}), nil
	// }}}
	// {{{ Applications
	case EApp:
		f := expr_to_normal(evaluator, inner.function^) or_return

		if f == nil {
			return nil, Evaluator_Error {
				loc = expr_pos(inner.function^),
				msg = "cannot apply expression",
			}
		}

		if prop, ok := expr_unparen(inner.argument^).(EProp); ok {
			prop_access := NProp_Access {
				object   = new_clone(f, evaluator.alloc),
				prop     = prop.name,
				prop_tok = prop.tok,
			}

			return NExpr(prop_access), nil
		}

		#partial switch _ in f {
		case EBool, EInt, EString, NList, NObject:
			return nil, Evaluator_Error {
				loc = expr_pos(inner.function^),
				msg = "expression is not callable",
			}
		}

		args: []NExpr

		if block, is_block := inner.argument^.(EBlock); is_block && block.kind == .Multi {
			args = multi_block_to_norm(evaluator, block) or_return
		} else {
			args = make_slice([]NExpr, 1)
			args[0] = expr_to_normal(evaluator, inner.argument^) or_return
		}

		napp := NApp {
			function  = new_clone(f, evaluator.alloc),
			arguments = args,
		}

		return napp, nil
	// }}}
	// {{{ Blocks
	case EBlock:
		if inner.args.names != nil {
			prev_effect := evaluator.current_effect
			defer evaluator.current_effect = prev_effect
			current_effect := make([dynamic]NStatement, 0, 4, evaluator.alloc)
			evaluator.current_effect = current_effect

			arg_names := make_slice([]Token, len(inner.args.names), evaluator.alloc)
			copy_slice(arg_names, inner.args.names)
			for arg, i in inner.args.names {
				arg_names[i].content = scope_push(evaluator, arg)
			}

			defer for _ in 0 ..< len(inner.args.names) {
				scope_pop(evaluator)
			}

			// Make a modifier version of the block, which takes no arguments
			copied := inner
			copied.args = {}
			norm_return := expr_to_normal(evaluator, Expr(copied)) or_return

			if norm_return == nil {
				return nil, Evaluator_Error {
					loc = expr_pos(copied),
					msg = "cannot turn expression into lambda body",
				}
			}

			body := NEffect {
				block_return = norm_return,
				contents     = evaluator.current_effect[:],
			}

			lam := NLambda {
				body = new_clone(body, evaluator.alloc),
				args = arg_names,
			}

			return lam, nil
		}

		pop_counter: uint = 0
		defer for _ in 0 ..< pop_counter {scope_pop(evaluator)}

		switch inner.kind {
		// {{{ Effects
		case .Effect:
			return_value: NExpr
			for st, i in inner.contents {
				switch st_inner in st {
				case ST_Globals:
					store_globals(evaluator, st_inner, &pop_counter) or_return
				case ST_Declaration:
					store_declaration(evaluator, st_inner, &pop_counter) or_return
				case ST_Assignment:
					return nil, Evaluator_Error {
						loc = st_inner.eq.from,
						msg = "assignments are not permitted inside effect blocks",
					}
				case ST_Expr:
					norm := expr_to_normal(evaluator, st_inner.expr^) or_return
					if i == len(inner.contents) - 1 {
						return_value = norm
					} else if napp, is_app := norm.(NApp); is_app {
						append(&evaluator.current_effect, NStatement(napp))
					} else {
						return nil, Evaluator_Error {
							loc = expr_pos(st_inner.expr^),
							msg = "this is not a standalone statement",
						}
					}
				}
			}

			return return_value, nil
		// }}}
		// {{{ Lists
		case .List:
			elements := make([dynamic]NExpr, 0, 4)

			for st, i in inner.contents {
				switch st_inner in st {
				case ST_Globals:
					store_globals(evaluator, st_inner, &pop_counter) or_return
				case ST_Declaration:
					store_declaration(evaluator, st_inner, &pop_counter) or_return
				case ST_Assignment:
					return nil, Evaluator_Error {
						loc = st_inner.eq.from,
						msg = "assignments are not permitted inside list blocks",
					}
				case ST_Expr:
					norm := expr_to_normal(evaluator, st_inner.expr^) or_return
					if norm != nil {append(&elements, norm)}
				}
			}

			return NList{elements = elements[:]}, nil
		// }}}
		// {{{ Objects
		case .Object:
			elements := new_clone(make([dynamic]NObject_Entry, 0, 4), evaluator.alloc)

			for st, i in inner.contents {
				switch st_inner in st {
				case ST_Globals:
					store_globals(evaluator, st_inner, &pop_counter) or_return
				case ST_Declaration:
					store_declaration(evaluator, st_inner, &pop_counter) or_return
				case ST_Expr:
					return nil, Evaluator_Error {
						loc = expr_pos(st_inner.expr^),
						msg = "standalone expressions are not permitted inside object blocks",
					}
				case ST_Assignment:
					norm := expr_to_normal(evaluator, st_inner.value) or_return

					if norm == nil {
						return nil, Evaluator_Error {
							loc = st_inner.eq.from,
							msg = "cannot assign invalid expression",
						}
					}

					object := elements
					steps: for step, i in st_inner.path_toks {
						// We look for the key inside the current object
						for entry in object {
							if entry.name == st_inner.path[i] {
								if i == len(st_inner.path_toks) - 1 {
									// If we found the key, then it's already been set, which is 
									// a name conflict
									context.temp_allocator = evaluator.alloc
									return nil, Evaluator_Error {
										loc = step.from,
										msg = fmt.tprintf("duplicate key %v", entry.name),
									}
								} else if obj, ok := entry.value.(NObject); ok {
									// We found the key, yet we have to keep going, and since 
									// this is an object, we do so
									object = obj.elements
									continue steps
								} else {
									// We have to keep going, but we can't, since this is not an 
									// object
									return nil, Evaluator_Error {
										loc = step.from,
										msg = fmt.tprintf(
											"cannot recursively assign to non-object",
										),
									}
								}
							}
						}

						if i == len(st_inner.path_toks) - 1 {
							// We haven't found the key, but that's a good thing, since we're 
							// at the end of the path, so that'd be a name conflict
							append(
								object,
								NObject_Entry {
									name_tok = step,
									name = st_inner.path[i],
									value = norm,
								},
							)
						} else {
							// First time encountering this key, but we have to keep going,
							// since we're not at the end of the path. Instead, we create a 
							// new object, and continue down the path into said object
							elements := new_clone(
								make([dynamic]NObject_Entry, 0, 4, evaluator.alloc),
								evaluator.alloc,
							)

							append(
								object,
								NObject_Entry {
									name_tok = step,
									name = st_inner.path[i],
									value = NObject{elements = elements},
								},
							)

							object = elements
						}
					}
				}
			}

			return NObject{elements = elements}, nil
		// }}}
		case .Multi:
			return nil, Evaluator_Error {
				loc = inner.kind_tok.from,
				msg = "invalid multi block encountered in the wild",
			}
		}
	// }}}
	}

	log.panicf("Unknown expression kind %v", expr)
}
