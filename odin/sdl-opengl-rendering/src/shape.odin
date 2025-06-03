package visuals

import "core:log"
import "core:math/linalg"
import "vendor:OpenGL"

// {{{ Command queues
Shape :: struct {
	z:    ℝ,
	fill: Color,
}

Rect :: struct {
	using shape: Shape,
	top_left:    ℝ²,
	dimensions:  ℝ²,
}

Circle :: struct {
	using shape: Shape,
	center:      ℝ²,
	radius:      ℝ,
}

Line :: struct {
	using shape: Shape,
	from:        ℝ²,
	to:          ℝ²,
	thickness:   ℝ,
}

Command_Queue :: struct {
	rects:   [dynamic]Rect,
	circles: [dynamic]Circle,
	lines:   [dynamic]Line,
}

queue: ^Command_Queue

init_command_queue :: proc() {
	queue = new(Command_Queue)
	queue^ = Command_Queue {
		rects   = make([dynamic]Rect),
		circles = make([dynamic]Circle),
		lines   = make([dynamic]Line),
	}
}

draw_rect_args :: proc(top_left: ℝ², dimensions: ℝ², color: Color, z: ℝ = 0) {
	draw_rect_struct(
		Rect{top_left = top_left, dimensions = dimensions, shape = Shape{z = z, fill = color}},
	)
}

draw_rect_struct :: proc(rect: Rect) {
	append(&queue.rects, rect)
}

draw_rect :: proc {
	draw_rect_struct,
	draw_rect_args,
}

draw_circle_args :: proc(center: ℝ², radius: ℝ, color: Color, z: ℝ = 0) {
	draw_circle_struct(
		Circle{center = center, radius = radius, shape = Shape{z = z, fill = color}},
	)
}

draw_circle_struct :: proc(circle: Circle) {
	append(&queue.circles, circle)
}

draw_circle :: proc {
	draw_circle_struct,
	draw_circle_args,
}

draw_line_args :: proc(from, to: ℝ², thickness: ℝ, color: Color, z: ℝ = 0) {
	draw_line_struct(
		Line{from = from, to = to, thickness = thickness, shape = Shape{z = z, fill = color}},
	)
}

draw_line_struct :: proc(line: Line) {
	append(&queue.lines, line)
}

draw_line :: proc {
	draw_line_struct,
	draw_line_args,
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
// {{{ Set transforms
// Commit the contents of `state.buf_matrices` and `state.buf_colors` to the 
// GPU.
commit_buffers :: proc(state: ^State, vao: ^VAO) {
	OpenGL.BindBuffer(OpenGL.ARRAY_BUFFER, vao.instance_mat_buffer)
	OpenGL.BufferData(
		OpenGL.ARRAY_BUFFER,
		INSTANCES * size_of(Mat3),
		&state.buf_matrices,
		OpenGL.DYNAMIC_DRAW,
	)

	OpenGL.BindBuffer(OpenGL.ARRAY_BUFFER, vao.instance_fill_buffer)
	OpenGL.BufferData(
		OpenGL.ARRAY_BUFFER,
		INSTANCES * size_of(Color),
		&state.buf_colors,
		OpenGL.DYNAMIC_DRAW,
	)
}

set_rect_transforms :: proc(state: ^State, vao: ^VAO, rects: []Rect) -> ℕ {
	log.assert(len(rects) <= INSTANCES, "Attempting to send too many rects to the GPU")
	for rect, i in rects {
		// This matrix must transform the rect [-1, 1]² into the desired rect
		mat: Mat3

		center := rect.top_left + rect.dimensions / 2
		mat[0, 0] = rect.dimensions.x / 2
		mat[1, 1] = rect.dimensions.y / 2
		mat[2].xy = center.xy
		mat[2].z = rect.z

		state.buf_matrices[i] = mat
		state.buf_colors[i] = rect.fill
	}

	return len(rects)
}

set_circle_transforms :: proc(state: ^State, vao: ^VAO, circles: []Circle) -> ℕ {
	log.assert(len(circles) <= INSTANCES, "Attempting to send too many circles to the GPU")

	for circle, i in circles {
		mat: Mat3

		mat[0, 0] = circle.radius
		mat[1, 1] = circle.radius
		mat[2].xy = circle.center.xy
		mat[2].z = circle.z

		state.buf_matrices[i] = mat
		state.buf_colors[i] = circle.fill
	}

	return len(circles)
}

set_line_transforms :: proc(state: ^State, vao: ^VAO, lines: []Line) -> ℕ {
	log.assert(len(lines) <= INSTANCES, "Attempting to send too many lines to the GPU")

	for line, i in lines {
		mat: Mat3

		dir := line.to - line.from
		mat[0].xy = dir / 2
		mat[1].xy = vec2_perp(linalg.normalize0(dir)) * line.thickness
		mat[2].xy = (line.from + line.to) / 2
		mat[2].z = line.z

		state.buf_matrices[i] = mat
		state.buf_colors[i] = line.fill
	}

	return len(lines)
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

clear_screen :: proc() {
	OpenGL.Clear(OpenGL.COLOR_BUFFER_BIT | OpenGL.DEPTH_BUFFER_BIT)
}

render_queue :: proc(state: ^State) {
	if state.wireframe {
		OpenGL.PolygonMode(OpenGL.FRONT_AND_BACK, OpenGL.LINE)
	} else {
		OpenGL.PolygonMode(OpenGL.FRONT_AND_BACK, OpenGL.FILL)
	}

	OpenGL.UseProgram(state.rect_program)
	for i := 0; i < len(queue.rects); i += INSTANCES {
		slice := queue.rects[i:]
		if len(slice) > INSTANCES {slice = slice[:INSTANCES]}
		instances := set_rect_transforms(state, &state.rect_vao, slice)
		commit_buffers(state, &state.rect_vao)
		draw_instances(state.rect_vao, instances)
	}
	clear(&queue.rects)

	OpenGL.UseProgram(state.circle_program)
	for i := 0; i < len(queue.circles); i += INSTANCES {
		slice := queue.circles[i:]
		if len(slice) > INSTANCES {slice = slice[:INSTANCES]}
		instances := set_circle_transforms(state, &state.rect_vao, slice)
		commit_buffers(state, &state.rect_vao)
		draw_instances(state.rect_vao, instances)
	}
	clear(&queue.circles)

	OpenGL.UseProgram(state.line_program)
	for i := 0; i < len(queue.lines); i += INSTANCES {
		slice := queue.lines[i:]
		if len(slice) > INSTANCES {slice = slice[:INSTANCES]}
		instances := set_line_transforms(state, &state.rect_vao, slice)
		commit_buffers(state, &state.rect_vao)
		draw_instances(state.rect_vao, instances)
	}
	clear(&queue.lines)

	OpenGL.UseProgram(0)
}
// }}}
