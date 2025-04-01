package visuals

import "core:log"
import "vendor:OpenGL"

// {{{ Math constants
ℝ :: f32
ℝ² :: [2]ℝ
ℝ³ :: [3]ℝ
ℕ :: uint
Mat3 :: matrix[3, 3]ℝ
Color :: [4]ℝ
// }}}
// {{{ Command queues
Rect :: struct {
	top_left:   ℝ²,
	dimensions: ℝ²,
	z:          ℝ,
	fill:       Color,
}

Circle :: struct {
	center: ℝ²,
	radius: ℝ,
}

Shape :: union {
	Rect,
	Circle,
}

Command_Queue :: struct {
	rects: [dynamic]Rect,
}

queue: ^Command_Queue

init_command_queue :: proc() {
	queue = new(Command_Queue)
	queue^ = Command_Queue {
		rects = make([dynamic]Rect),
	}
}

draw_rect_args :: proc(top_left: ℝ², dimensions: ℝ², color: Color, z: ℝ = 0) {
	draw_rect_struct(Rect{top_left, dimensions, z, color})
}

draw_rect_struct :: proc(rect: Rect) {
	append(&queue.rects, rect)
}

draw_rect :: proc {
	draw_rect_args,
	draw_rect_struct,
}

// }}}

// {{{ VAO & consts
VAO :: struct {
	vao:                  u32,
	ibo:                  u32,
	vertex_pos_buffer:    u32,
	instance_fill_buffer: u32,
	instance_mat_buffer:  u32,
	index_count:          ℕ,
}

INSTANCES :: 1024 // The number of instances to allocate space in the buffer for
VERTEX_POS_LOCATION :: 0
INSTANCE_FILL_LOCATION :: 1
INSTANCE_MAT_LOCATION :: 2
// }}}
// {{{ Create VAO
create_vao :: proc(vertices: []ℝ², indices: []u32) -> (out: VAO, ok: bool) {
	out.index_count = len(indices)

	// Create VAO
	OpenGL.GenVertexArrays(1, &out.vao)
	OpenGL.BindVertexArray(out.vao)

	// Create instance fill VBO
	OpenGL.GenBuffers(1, auto_cast &out.instance_fill_buffer)
	OpenGL.BindBuffer(OpenGL.ARRAY_BUFFER, out.instance_fill_buffer)

	OpenGL.EnableVertexAttribArray(INSTANCE_FILL_LOCATION)
	OpenGL.VertexAttribPointer(INSTANCE_FILL_LOCATION, 4, OpenGL.FLOAT, false, size_of(Color), 0)
	OpenGL.VertexAttribDivisor(INSTANCE_FILL_LOCATION, 1)

	// Create instance mat VBO
	OpenGL.GenBuffers(1, auto_cast &out.instance_mat_buffer)
	OpenGL.BindBuffer(OpenGL.ARRAY_BUFFER, out.instance_mat_buffer)

	for i in u32(0) ..< 3 {
		OpenGL.EnableVertexAttribArray(INSTANCE_MAT_LOCATION + i)
		vec3_size :: size_of(ℝ³)
		OpenGL.VertexAttribPointer(
			INSTANCE_MAT_LOCATION + i,
			3,
			OpenGL.FLOAT,
			false,
			3 * vec3_size,
			uintptr(i * vec3_size),
		)
		OpenGL.VertexAttribDivisor(INSTANCE_MAT_LOCATION + i, 1)
	}

	//Create position VBO
	OpenGL.GenBuffers(1, &out.vertex_pos_buffer)
	OpenGL.BindBuffer(OpenGL.ARRAY_BUFFER, out.vertex_pos_buffer)
	OpenGL.BufferData(
		OpenGL.ARRAY_BUFFER,
		len(vertices) * 2 * size_of(ℝ),
		raw_data(vertices),
		OpenGL.STATIC_DRAW,
	)

	// Put the data into the VBO
	OpenGL.EnableVertexAttribArray(VERTEX_POS_LOCATION)
	OpenGL.VertexAttribPointer(VERTEX_POS_LOCATION, 2, OpenGL.FLOAT, false, 2 * size_of(ℝ), 0)

	// Create the IBO
	OpenGL.GenBuffers(1, auto_cast &out.ibo)
	OpenGL.BindBuffer(OpenGL.ELEMENT_ARRAY_BUFFER, out.ibo)
	OpenGL.BufferData(
		OpenGL.ELEMENT_ARRAY_BUFFER,
		len(indices) * size_of(u32),
		raw_data(indices),
		OpenGL.STATIC_DRAW,
	)

	OpenGL.BindVertexArray(0)

	return out, true
}
// }}}
// {{{ Set rect transforms
set_rect_transforms :: proc(vao: ^VAO, rects: []Rect) -> ℕ {
	log.assert(len(rects) <= INSTANCES, "Attempting to send too many rects to the GPU")
	matrices := new([INSTANCES]Mat3, context.temp_allocator)
	fills := new([INSTANCES]Color, context.temp_allocator)

	for rect, i in rects {
		// This matrix must transform the rect [-1, 1]² into the desired rect
		mat: Mat3

		center := rect.top_left + rect.dimensions / 2
		mat[0, 0] = rect.dimensions.x / 2
		mat[1, 1] = rect.dimensions.y / 2
		mat[2].xy = center.xy
		mat[2].z = rect.z

		matrices[i] = mat
		fills[i] = rect.fill
	}

	OpenGL.BindBuffer(OpenGL.ARRAY_BUFFER, vao.instance_mat_buffer)
	OpenGL.BufferData(
		OpenGL.ARRAY_BUFFER,
		INSTANCES * size_of(Mat3),
		matrices,
		OpenGL.DYNAMIC_DRAW,
	)
	OpenGL.BindBuffer(OpenGL.ARRAY_BUFFER, vao.instance_fill_buffer)
	OpenGL.BufferData(OpenGL.ARRAY_BUFFER, INSTANCES * size_of(Color), fills, OpenGL.DYNAMIC_DRAW)

	return len(rects)
}
// }}}
// {{{ Render the entire queue
draw_instances :: proc(vao: VAO, instances: ℕ) {
	OpenGL.BindVertexArray(vao.vao)
	OpenGL.DrawElementsInstanced(
		OpenGL.TRIANGLE_FAN,
		i32(vao.index_count),
		OpenGL.UNSIGNED_INT,
		nil,
		i32(instances),
	)
}

render_queue :: proc(state: ^State) {
	rect_steps := len(queue.rects) / INSTANCES

	for i := 0; i < len(queue.rects); i += INSTANCES {
		slice := queue.rects[i:]
		if len(slice) > INSTANCES {slice = slice[:INSTANCES]}
		instances := set_rect_transforms(&state.rect_vao, slice)
		draw_instances(state.rect_vao, instances)
	}

	clear(&queue.rects)
}
// }}}
