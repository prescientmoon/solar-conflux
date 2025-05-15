package enfold

import "core:log"
import "core:os"

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
