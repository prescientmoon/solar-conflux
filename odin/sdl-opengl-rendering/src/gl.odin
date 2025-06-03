package visuals

import "base:intrinsics"
import "base:runtime"
import "core:reflect"
import "vendor:OpenGL"

GL_Buffer_Usage :: enum u32 {
	Static  = OpenGL.STATIC_DRAW,
	Dynamic = OpenGL.DYNAMIC_DRAW,
}

GL_Buffer_Kind :: enum u32 {
	Array         = OpenGL.ARRAY_BUFFER,
	Element_Array = OpenGL.ELEMENT_ARRAY_BUFFER,
	Uniform       = OpenGL.UNIFORM_BUFFER,
}

// {{{ Set array buffer
@(private)
gl_sizeof_ptr :: proc(ptr: ^$T) -> int {
	return size_of(T)
}

@(private)
gl_sizeof_slice :: proc(slice: []$T) -> int {
	return len(slice) * size_of(T)
}

@(private)
gl_sizeof :: proc {
	gl_sizeof_ptr,
	gl_sizeof_slice,
}

@(private)
gl_to_rawptr_ptr :: proc(ptr: ^$T) -> rawptr {
	return ptr
}

@(private)
gl_to_rawptr_slice :: proc(slice: []$T) -> rawptr {
	return raw_data(slice)
}

@(private)
gl_to_rawptr :: proc {
	gl_to_rawptr_ptr,
	gl_to_rawptr_slice,
}

bind_buffer :: proc(id: u32, buffer: GL_Buffer_Kind = .Array) {
	OpenGL.BindBuffer(u32(buffer), id)
}

set_buffer :: proc(
	id: u32,
	data: $T,
	buffer: GL_Buffer_Kind = .Array,
	usage: GL_Buffer_Usage = .Dynamic,
) {
	bind_buffer(id, buffer)
	OpenGL.BufferData(u32(buffer), gl_sizeof(data), gl_to_rawptr(data), u32(usage))
}

gen_buffer :: proc() -> (out: u32) {
	OpenGL.GenBuffers(1, &out)
	return out
}
// }}}
// {{{ Drawing commands
clear_screen :: proc() {
	OpenGL.Clear(OpenGL.COLOR_BUFFER_BIT | OpenGL.DEPTH_BUFFER_BIT)
}
// }}}


GL_Base_Type :: enum {
	Float,
}

setup_vbo :: proc(
	id: u32,
	location: u32,
	ty: GL_Base_Type = .Float,
	rows: i32 = 1,
	cols: i32 = 1,
	divisor := 0,
) {
	bind_buffer(id, .Array)

	elem_size: i32
	base: u32

	switch ty {
	case .Float:
		elem_size = i32(size_of(f32))
		base = OpenGL.FLOAT
	}

	offset: i32 = 0

	for i in u32(0) ..< u32(cols) {
		OpenGL.EnableVertexAttribArray(location + i)
		OpenGL.VertexAttribPointer(
			location + i,
			rows,
			OpenGL.FLOAT,
			false,
			i32(cols * rows) * elem_size,
			uintptr(offset),
		)

		offset += rows * elem_size

		if divisor != 0 {
			OpenGL.VertexAttribDivisor(location + i, 1)
		}
	}
}
