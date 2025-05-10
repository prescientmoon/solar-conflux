package enfold

import "core:fmt"
import "core:io"

Bytes :: distinct uint

@(init)
init_formatters :: proc() {
	fmt.set_user_formatters(new(map[typeid]fmt.User_Formatter))

	// {{{ Bytes
	fmt.register_user_formatter(Bytes, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		bytes := cast(^Bytes)arg.data

		switch verb {
		case 'v':
			units := [?]string{"B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"}

			unit_index := 0
			size := f32(bytes^)

			for size >= 1024 && unit_index < len(units) - 1 {
				size /= 1024.0
				unit_index += 1
			}

			fmt.wprintf(fi.writer, "%.2f%s", size, units[unit_index])

			return true
		case:
			return false
		}
	})
	// }}}
	// {{{ Source locations
	fmt.register_user_formatter(Source_Loc, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		source_loc := cast(^Source_Loc)arg.data

		switch verb {
		case 'v':
			fmt.wprintf(fi.writer, "%v:%v", source_loc.line, source_loc.col)

			return true
		case:
			return false
		}
	})
	// }}}
	// {{{ Tokens
	fmt.register_user_formatter(Token, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		tok := cast(^Token)arg.data

		switch verb {
		case 'v':
			fmt.wprintf(fi.writer, "%v(%v, \"%v\")", tok.kind, tok.from, tok.content)

			return true
		case:
			return false
		}
	})
	// }}}
	// {{{ EInt
	fmt.register_user_formatter(EInt, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		expr := cast(^EInt)arg.data

		switch verb {
		case 'v':
			fmt.wprintf(fi.writer, "%v", expr.value)

			return true
		case:
			return false
		}
	})
	// }}}
	// {{{ EString
	fmt.register_user_formatter(EString, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		expr := cast(^EString)arg.data

		switch verb {
		case 'v':
			fmt.wprintf(fi.writer, "\"%v\"", expr.value)

			return true
		case:
			return false
		}
	})
	// }}}
	// {{{ EVar
	fmt.register_user_formatter(EVar, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		expr := cast(^EVar)arg.data

		switch verb {
		case 'v':
			fmt.wprintf(fi.writer, "%v", expr.name.content)

			return true
		case:
			return false
		}
	})
	// }}}
	// {{{ EProp
	fmt.register_user_formatter(EProp, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		expr := cast(^EProp)arg.data

		switch verb {
		case 'v':
			fmt.wprintf(fi.writer, ".%v", expr.name)

			return true
		case:
			return false
		}
	})
	// }}}
	// {{{ EApp
	fmt.register_user_formatter(EApp, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		expr := cast(^EApp)arg.data

		switch verb {
		case 'v':
			fmt.wprintf(fi.writer, "App(%v, %v)", expr.function^, expr.argument^)

			return true
		case:
			return false
		}
	})
	// }}}
	// {{{ ST_Expr
	fmt.register_user_formatter(ST_Expr, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		st := cast(^ST_Expr)arg.data

		switch verb {
		case 'v':
			fmt.wprintf(fi.writer, "%v", st.expr)

			return true
		case:
			return false
		}
	})
	// }}}
	// {{{ ST_Assignment
	fmt.register_user_formatter(ST_Assignment, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		st := cast(^ST_Assignment)arg.data

		switch verb {
		case 'v':
			fmt.wprintf(fi.writer, "%v", st.path[0])
			for v in st.path[1:] {
				fmt.wprintf(fi.writer, ".%v", v)
			}

			fmt.wprintf(fi.writer, " = %v", st.value)

			return true
		case:
			return false
		}
	})
	// }}}
	// {{{ ST_Declaration
	fmt.register_user_formatter(ST_Declaration, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		st := cast(^ST_Declaration)arg.data

		switch verb {
		case 'v':
			for v, i in st.vars {
				fmt.wprintf(fi.writer, "%v", v.content)
				if i != len(st.vars) - 1 {fmt.wprintf(fi.writer, ", ")}
			}

			fmt.wprintf(fi.writer, " := %v", st.value)

			return true
		case:
			return false
		}
	})
	// }}}
	// {{{ Block_Args
	fmt.register_user_formatter(Block_Args, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		args := cast(^Block_Args)arg.data

		switch verb {
		case 'v':
			fmt.wprintf(fi.writer, "[")

			for name, i in args.names {
				fmt.wprintf(fi.writer, "%v", name.content)

				if i != len(args.names) - 1 {
					fmt.wprintf(fi.writer, ", ")
				}
			}

			fmt.wprintf(fi.writer, "]")

			return true
		case:
			return false
		}
	})
	// }}}
}

@(fini)
deinit_formatters :: proc() {
	delete(fmt._user_formatters^)
	free(fmt._user_formatters)
}
