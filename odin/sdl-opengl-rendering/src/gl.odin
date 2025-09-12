package visuals

import "base:intrinsics"
import "base:runtime"
import "core:log"
import "core:reflect"
import "vendor:OpenGL"

// {{{ GL types
GL_Buffer_Usage :: enum u32 {
	Static  = OpenGL.STATIC_DRAW,
	Dynamic = OpenGL.DYNAMIC_DRAW,
}

GL_Buffer_Kind :: enum u32 {
	Array         = OpenGL.ARRAY_BUFFER,
	Element_Array = OpenGL.ELEMENT_ARRAY_BUFFER,
	Uniform       = OpenGL.UNIFORM_BUFFER,
}

GL_Base_Type :: enum {
	Float,
}

GL_UBO :: u32
GL_FBO :: u32
GL_TEX :: u32
GL_VAO :: u32
GL_BUF :: u32
GL_LOC :: u32
GL_BINDING :: u32
GL_PROGRAM :: u32
// }}}
// {{{ Set array buffer
gen_buffer :: proc() -> (out: GL_BUF) {
	OpenGL.GenBuffers(1, &out)
	return out
}

bind_buffer :: proc(id: GL_BUF, buffer: GL_Buffer_Kind = .Array) {
	OpenGL.BindBuffer(u32(buffer), id)
}

set_buffer :: proc(
	id: GL_BUF,
	data: $T,
	buffer: GL_Buffer_Kind = .Array,
	usage: GL_Buffer_Usage = .Dynamic,
	loc := #caller_location,
) {
	bind_buffer(id, buffer)

	data := data
	size: int
	ptr: rawptr
	when intrinsics.type_is_slice(T) {
		size = len(data) * size_of(intrinsics.type_elem_type(T))
		ptr = raw_data(data)
	} else when intrinsics.type_is_pointer(T) {
		size = size_of(intrinsics.type_elem_type(T))
		ptr = data
	} else {
		size = size_of(T)
		ptr = &data
	}

	OpenGL.BufferData(u32(buffer), size, ptr, u32(usage))
}
// }}}
// {{{ VBO setup
setup_vbo :: proc(
	id: GL_BUF,
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
// }}}
// {{{ Drawing commands
clear_screen :: proc() {
	OpenGL.Clear(OpenGL.COLOR_BUFFER_BIT | OpenGL.DEPTH_BUFFER_BIT)
}
// }}}
