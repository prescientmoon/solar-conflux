package visuals

import "core:log"
import "core:math/linalg"
import "vendor:OpenGL"

// {{{ Shape Types
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

Rounded_Line :: distinct Line
// }}}
// {{{ Command queues
Command_Queue :: struct {
	rects:         [dynamic]Rect,
	circles:       [dynamic]Circle,
	lines:         [dynamic]Line,
	rounded_lines: [dynamic]Rounded_Line,
}

queue: ^Command_Queue

init_command_queue :: proc() {
	queue = new(Command_Queue)
	queue^ = Command_Queue {
		rects         = make([dynamic]Rect),
		circles       = make([dynamic]Circle),
		lines         = make([dynamic]Line),
		rounded_lines = make([dynamic]Rounded_Line),
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

draw_rounded_line_args :: proc(from, to: ℝ², thickness: ℝ, color: Color, z: ℝ = 0) {
	draw_rounded_line_struct(
		Line{from = from, to = to, thickness = thickness, shape = Shape{z = z, fill = color}},
	)
}

draw_rounded_line_struct :: proc(line: $T/Line) {
	append(&queue.rounded_lines, Rounded_Line(line))
}

draw_rounded_line :: proc {
	draw_rounded_line_struct,
	draw_rounded_line_args,
}
// }}}
// {{{ Shape -> Transform
to_transform_rect :: proc(rect: Rect) -> (mat: Mat3) {
	center := rect.top_left + rect.dimensions / 2
	mat[0, 0] = rect.dimensions.x / 2
	mat[1, 1] = rect.dimensions.y / 2
	mat[2, 2] = 1
	mat[2].xy = center.xy
	return mat
}

to_transform_circle :: proc(circle: Circle) -> (mat: Mat3) {
	mat[0, 0] = circle.radius
	mat[1, 1] = circle.radius
	mat[2, 2] = 1
	mat[2].xy = circle.center.xy
	return mat
}

to_transform_line :: proc(line: Line) -> (mat: Mat3) {
	dir := line.to - line.from
	mat[0].xy = dir / 2
	mat[1].xy = vec2_perp(linalg.normalize0(dir)) * line.thickness
	mat[2].xy = (line.from + line.to) / 2
	mat[2].z = line.z
	return mat
}

to_transform_rounded_line :: proc(line: Rounded_Line) -> (mat: Mat3) {
	dir := line.to - line.from
	len := linalg.length(dir) // TODO: return if this is close to 0

	mat[0].xy = dir * (len + line.thickness * 2) / len / 2
	mat[1].xy = vec2_perp(dir / len) * line.thickness
	mat[2].xy = (line.from + line.to) / 2
	mat[2].z = line.z
	return mat
}

// This matrix must transform [-1, 1]² into the desired rect
to_transform :: proc {
	to_transform_rect,
	to_transform_circle,
	to_transform_line,
	to_transform_rounded_line,
}
// }}}

// {{{ GPU data types
VAO :: struct {
	vao:                       u32,
	vertex_ind_buffer:         u32,
	vertex_pos_buffer:         u32,
	index_count:               ℕ,

	// Per instance attributes
	instance_fill_buffer:      u32,
	instance_mat_buffer:       u32,

	// Optional
	instance_line_buffer:      u32,
	instance_thickness_buffer: u32,
}


// odinfmt: disable
RMat3 ::   matrix[4, 4]f32
// odinfmt: enable

UBO :: u32
Global_Uniforms :: struct {
	viewport_matrix: RMat3,
	aaWidth:         f32,
}

Program :: u32

INSTANCES :: 1024 // The number of instances to allocate space in the buffer for
VERTEX_POS_LOCATION :: 0
INSTANCE_FILL_LOCATION :: 1
INSTANCE_MAT_LOCATION :: 2
INSTANCE_LINE_LOCATION :: 5
INSTANCE_THICKNESS_LOCATION :: 7
ubo_globals_BINDING :: 0
// }}}
// {{{ Create globals UBO
create_ubo_globals :: proc() -> UBO {
	id := gen_buffer()

	set_buffer(id, &Global_Uniforms{}, buffer = .Uniform)
	OpenGL.BindBufferBase(OpenGL.UNIFORM_BUFFER, ubo_globals_BINDING, id)

	return id
}
// }}}
// {{{ Create VAO
NDC_RECT_VERTICES: [4]ℝ² : {{-1, -1}, {1, -1}, {1, 1}, {-1, 1}}
NDC_RECT_INDICES: [4]u32 : {0, 1, 2, 3}

create_vao :: proc(out: ^VAO) {
	out.index_count = len(NDC_RECT_INDICES)

	// Create VAO
	OpenGL.GenVertexArrays(1, &out.vao)
	OpenGL.BindVertexArray(out.vao)
	defer OpenGL.BindVertexArray(0)

	// Create instance fill VBO
	if out.instance_fill_buffer == 0 do out.instance_fill_buffer = gen_buffer()
	bind_buffer(out.instance_fill_buffer, .Array)
	setup_vbo(out.instance_fill_buffer, INSTANCE_FILL_LOCATION, rows = 4, divisor = 1)

	// Create instance mat VBO
	if out.instance_mat_buffer == 0 do out.instance_mat_buffer = gen_buffer()
	setup_vbo(
		out.instance_mat_buffer,
		INSTANCE_MAT_LOCATION,
		ty = .Float,
		rows = 3,
		cols = 3,
		divisor = 1,
	)

	// Create position VBO
	if out.vertex_pos_buffer == 0 do out.vertex_pos_buffer = gen_buffer()
	set_buffer(out.vertex_pos_buffer, NDC_RECT_VERTICES, usage = .Static)
	setup_vbo(out.vertex_pos_buffer, VERTEX_POS_LOCATION, ty = .Float, rows = 2)

	// Create the vertex_ind_buffer
	if out.vertex_ind_buffer == 0 do out.vertex_ind_buffer = gen_buffer()
	set_buffer(out.vertex_ind_buffer, NDC_RECT_INDICES, buffer = .Element_Array, usage = .Static)
}

create_rounded_line_vao :: proc(state: ^State) {
	vao := &state.rounded_line_vao
	// Create VAO
	vao^ = state.rect_vao
	create_vao(vao)

	OpenGL.BindVertexArray(vao.vao)
	defer OpenGL.BindVertexArray(0)

	// Create instance line VBO
	vao.instance_line_buffer = gen_buffer()
	bind_buffer(vao.instance_line_buffer, .Array)
	setup_vbo(vao.instance_line_buffer, INSTANCE_LINE_LOCATION, rows = 2, cols = 2, divisor = 1)

	vao.instance_thickness_buffer = gen_buffer()
	bind_buffer(vao.instance_thickness_buffer, .Array)
	setup_vbo(vao.instance_thickness_buffer, INSTANCE_THICKNESS_LOCATION, divisor = 1)
}
// }}}

// {{{ Render the entire queue
render_instanced :: proc(state: ^State, vao: VAO, shapes: ^[dynamic]$T) {
	OpenGL.BindVertexArray(vao.vao)

	steps := len(shapes) / INSTANCES

	for i := 0; i < len(shapes); i += INSTANCES {
		slice := shapes[i:]
		if len(slice) > INSTANCES {slice = slice[:INSTANCES]}

		for shape, i in shapes {
			state.buf_matrices[i] = to_transform(shape)
			state.buf_matrices[i][2, 2] = shape.z
			state.buf_colors[i] = shape.fill
		}

		set_buffer(vao.instance_mat_buffer, &state.buf_matrices)
		set_buffer(vao.instance_fill_buffer, &state.buf_colors)

		when T == Rounded_Line {
			for shape, i in shapes {
				state.buf_lines[i][0] = shape.from
				state.buf_lines[i][1] = shape.to
				state.buf_thicknesses[i] = shape.thickness
			}

			set_buffer(vao.instance_line_buffer, &state.buf_lines)
			set_buffer(vao.instance_thickness_buffer, &state.buf_thicknesses)
		}

		OpenGL.DrawElementsInstanced(
			OpenGL.TRIANGLE_FAN,
			i32(vao.index_count),
			OpenGL.UNSIGNED_INT,
			nil,
			i32(len(shapes)),
		)
	}

	clear(shapes)
}

render_queue :: proc(state: ^State) {
	// Update uniform data
	set_buffer(state.ubo_globals, &state.globals, buffer = .Uniform)

	// Toggle the wireframe
	OpenGL.PolygonMode(OpenGL.FRONT_AND_BACK, state.wireframe ? OpenGL.LINE : OpenGL.FILL)

	OpenGL.UseProgram(state.rect_program)
	render_instanced(state, state.rect_vao, &queue.rects)

	OpenGL.UseProgram(state.circle_program)
	render_instanced(state, state.rect_vao, &queue.circles)

	OpenGL.UseProgram(state.line_program)
	render_instanced(state, state.rect_vao, &queue.lines)

	OpenGL.UseProgram(state.rounded_line_program)
	render_instanced(state, state.rounded_line_vao, &queue.rounded_lines)

	OpenGL.UseProgram(0)
}
// }}}
