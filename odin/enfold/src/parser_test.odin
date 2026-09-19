package enfold

import "core:fmt"
import "core:log"
import "core:mem/virtual"
import "core:os"
import "core:testing"

test_parser :: proc(
	path: string,
	parse: $T/(proc(_: ^Parser) -> ($E, Enfold_Error)),
	should_pass: bool,
) {
	files := ls(path)
	defer os.file_info_slice_delete(files)

	for file in files {
		defer free_all(context.temp_allocator)

		handle := open(file.fullpath)
		defer os.close(handle)

		source := read(file.fullpath, handle)
		defer delete(source)

		parser, err := mk_parser(source, context.temp_allocator)
		if err != nil {log.panic(err)}

		expr: Expr
		expr, err = parse(&parser)

		if should_pass && err != nil {
			log.panicf("Failed to parse %v: \n%#v", file.name, err)
		} else if !should_pass && err == nil {
			log.panicf("expression %v parsed succesfully: \n%#v", file.name, expr)
		}

		log.infof("✅ %v", file.name)
	}
}


@(test)
test_toplevel_expr_good :: proc(t: ^testing.T) {
	test_parser("test/expr/good", parse_toplevel_expr, true)
}

@(test)
test_toplevel_expr_bad :: proc(t: ^testing.T) {
	// context.logger.options -= {.Short_File_Path, .Line}
	test_parser("test/expr/bad", parse_toplevel_expr, false)
}
