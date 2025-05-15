package enfold

import "base:runtime"
import "core:fmt"
import "core:strings"

Codegen :: struct {
	out: strings.Builder,
}

mk_lua_codegen :: proc(alloc: runtime.Allocator) -> (cg: Codegen) {
	cg.out = strings.builder_make(0, 65536, alloc)
	return
}

expr_to_lua :: proc(cg: ^Codegen, expr: NExpr) {
	switch inner in expr {
	case NVar:
		strings.write_string(&cg.out, inner.name)
	case EString:
		strings.write_quoted_string(&cg.out, inner.value)
	case EBool:
		fmt.sbprintf(&cg.out, "%v", inner.value)
	case EInt:
		fmt.sbprintf(&cg.out, "%v", inner.value)
	case NList:
		strings.write_rune(&cg.out, '{')

		for e, i in inner.elements {
			expr_to_lua(cg, e)

			if i != len(inner.elements) - 1 {
				strings.write_rune(&cg.out, ',')
			}
		}

		strings.write_rune(&cg.out, '}')
	case NObject:
		strings.write_rune(&cg.out, '{')

		for e, i in inner.elements {
			fmt.sbprintf(&cg.out, "%v = ", e.name)
			expr_to_lua(cg, e.value)

			if i != len(inner.elements) - 1 {
				strings.write_rune(&cg.out, ',')
			}
		}

		strings.write_rune(&cg.out, '}')
	case NLambda:
		strings.write_string(&cg.out, "function(")

		for e, i in inner.args {
			strings.write_string(&cg.out, e.content)

			if i != len(inner.args) - 1 {
				strings.write_rune(&cg.out, ',')
			}
		}

		strings.write_string(&cg.out, ")\n")
		effect_to_lua(cg, inner.body^)
		strings.write_string(&cg.out, "end")
	case NApp:
		needs_parens := false

		#partial switch _ in inner.function {
		case NLambda:
			needs_parens = true
		}

		if needs_parens {strings.write_rune(&cg.out, '(')}
		expr_to_lua(cg, inner.function^)
		if needs_parens {strings.write_rune(&cg.out, ')')}

		strings.write_rune(&cg.out, '(')
		for e, i in inner.arguments {
			expr_to_lua(cg, e)

			if i != len(inner.arguments) - 1 {
				strings.write_rune(&cg.out, ',')
			}
		}

		strings.write_rune(&cg.out, ')')
	case NProp_Access:
		needs_parens := false

		#partial switch _ in inner.object {
		case NObject, NList, NLambda:
			needs_parens = true
		}

		if needs_parens {strings.write_rune(&cg.out, '(')}
		expr_to_lua(cg, inner.object^)
		if needs_parens {strings.write_rune(&cg.out, ')')}
		fmt.sbprintf(&cg.out, ".%v", inner.prop)
	}
}

effect_to_lua :: proc(cg: ^Codegen, expr: NEffect) {
	for statement in expr.contents {
		switch inner in statement {
		case NST_Declaration:
			for v, i in inner.vars {
				strings.write_string(&cg.out, v.content)

				if i != len(inner.vars) - 1 {
					strings.write_rune(&cg.out, ',')
				}
			}

			strings.write_rune(&cg.out, '=')
			expr_to_lua(cg, inner.value)

		case NApp:
			expr_to_lua(cg, inner)
		}

		strings.write_rune(&cg.out, '\n')
	}

	if expr.block_return != nil {
		strings.write_string(&cg.out, "return ")
		expr_to_lua(cg, expr.block_return)
		strings.write_rune(&cg.out, '\n')
	}
}
