package enfold

import "core:fmt"
import "core:log"
import "core:mem/virtual"
import "core:os"
import "core:testing"

open :: proc(path: string) -> os.Handle {
	handle, err := os.open(path)
	if err != nil {log.panicf("Failed to open file/directory %v: %v", path, err)}
	return handle
}

read :: proc(path: string, handle: os.Handle) -> string {
	content, err := os.read_entire_file_from_handle_or_err(handle)
	if err != nil {log.panicf("Failed to read file %v: %v", path, err)}
	return string(content)
}

ls :: proc(path: string) -> []os.File_Info {
	handle := open(path)
	defer os.close(handle)

	files, err := os.read_dir(handle, -1)
	if err != nil {log.panicf("Failed to read directory %v: %v", path, err)}
	return files
}

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
