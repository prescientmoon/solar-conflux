package visuals

import "core:fmt"
import "core:log"
import "core:math"
import "core:math/linalg"
import "core:math/linalg/glsl"
import "core:slice"
import "core:strings"
import "vendor:OpenGL"

// {{{ Shape Types
Shape_Options :: struct {
	z:            ℝ,
	fill:         Color,
	stroke:       Color,
	stroke_width: ℝ,
}

Shape :: struct(T: typeid) {
	using opts: Shape_Options,
	using data: T,
}

Line :: struct {
	from:      ℝ²,
	to:        ℝ²,
	thickness: ℝ,
}

Rounded_Line :: distinct Line
// }}}
// {{{ Shape -> Transform
to_transform_rect :: proc(rect: Shape(□)) -> (mat: Affine2) {
	mat[0, 0] = rect.dimensions.x + rect.stroke_width * 2
	mat[1, 1] = rect.dimensions.y + rect.stroke_width * 2
	mat[2].xy = rect.top_left - rect.stroke_width
	return mat
}

to_transform_circle :: proc(circle: Shape(Circle2)) -> (mat: Affine2) {
	r := circle.radius + circle.stroke_width
	mat[0, 0] = r * 2
	mat[1, 1] = r * 2
	mat[2].xy = circle.center - r
	return mat
}

to_transform_line :: proc(line: Line) -> (mat: Affine2) {
	dir := line.to - line.from
	mat[0].xy = dir / 2
	mat[1].xy = vec2_perp(linalg.normalize0(dir)) * line.thickness
	mat[2].xy = (line.from + line.to) / 2
	return mat
}

to_transform_rounded_line :: proc(line: Shape(Rounded_Line)) -> (mat: Affine2) {
	dir := line.to - line.from
	len := linalg.length(dir)
	if ≃(len, 0) {return}

	thickness := line.thickness + line.stroke_width
	mat[0].xy = dir * (len + thickness * 2) / len
	mat[1].xy = 2 * vec2_perp(dir / len) * thickness
	mat[2].xy = (line.from + line.to - mat[0].xy - mat[1].xy) / 2
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
// {{{ Drawing commands
draw_rect_args :: proc(top_left: ℝ², dimensions: ℝ², opts: Shape_Options) {
	draw_rect_struct(□{top_left = top_left, dimensions = dimensions}, opts)
}

draw_rect_struct :: proc(aabb: □, opts: Shape_Options) {
	append(&g_renderer_state().q_rects, Shape(□){data = aabb, opts = opts})
}

draw_rect :: proc {
	draw_rect_struct,
	draw_rect_args,
}

draw_circle_args :: proc(center: ℝ², radius: ℝ, opts: Shape_Options) {
	draw_circle_struct(Circle2{center = center, radius = radius}, opts)
}

draw_circle_struct :: proc(circle: Circle2, opts: Shape_Options) {
	append(&g_renderer_state().q_circles, Shape(Circle2){data = circle, opts = opts})
}

draw_circle :: proc {
	draw_circle_struct,
	draw_circle_args,
}

draw_line_args :: proc(from, to: ℝ², thickness: ℝ, opts: Shape_Options, rounded := true) {
	draw_line_struct(Line{from = from, to = to, thickness = thickness}, opts, rounded)
}

draw_line_struct :: proc(line: Line, opts: Shape_Options, rounded := true) {
	if rounded {
		append(
			&g_renderer_state().q_rounded_lines,
			Shape(Rounded_Line){data = Rounded_Line(line), opts = opts},
		)
	} else {
		append(&g_renderer_state().q_lines, Shape(Line){data = line, opts = opts})
	}
}

draw_line :: proc {
	draw_line_struct,
	draw_line_args,
}
// }}}

// {{{ Uniforms
UBO_ID :: enum {
	Globals,
	JFA,
}

@(rodata)
UBO_ID_BINDING: [UBO_ID]GL_BINDING = {
	.Globals = 0,
	.JFA     = 1,
}

Global_Uniforms :: struct {
	viewport_matrix: Mat4,
	aaWidth:         f32,
}

JFA_Uniforms :: struct {
	offset:     f32,
	_:          f32, // padding
	resolution: ℝ²,
}
// }}}
// {{{ VBOS / Programs
VERTEX_POS_LOCATION :: 0
INSTANCES :: 1024 // The number of instances to allocate space for
Instance_Param_Buf :: enum {
	Fill,
	Stroke,
	Stroke_Width,
	Z_Offset,
	Model_Mat,
	Dimensions,
	Center,
	Radius,
	Line,
	Thickness,
}

@(rodata)
INSTANCE_PARAM_LOCATIONS: [Instance_Param_Buf]GL_LOC = {
	.Fill         = 1,
	.Stroke       = 2,
	.Stroke_Width = 3,
	.Z_Offset     = 4,
	.Model_Mat    = 5,
	.Center       = 8,
	.Dimensions   = 9,
	.Radius       = 9,
	.Line         = 8,
	.Thickness    = 10,
}

@(rodata)
INSTANCE_PARAM_TYPE: [Instance_Param_Buf]string = {
	.Fill         = "vec4",
	.Stroke       = "vec4",
	.Stroke_Width = "float",
	.Z_Offset     = "float",
	.Model_Mat    = "mat3x2",
	.Dimensions   = "vec2",
	.Center       = "vec2",
	.Radius       = "float",
	.Line         = "vec2[2]",
	.Thickness    = "float",
}

@(rodata)
INSTANCE_PARAM_DIMS: [Instance_Param_Buf][2]i32 = { 	// (rows, cols)
	.Fill         = {4, 1},
	.Stroke       = {4, 1},
	.Stroke_Width = {1, 1},
	.Z_Offset     = {1, 1},
	.Model_Mat    = {2, 3},
	.Dimensions   = {2, 1},
	.Center       = {2, 1},
	.Radius       = {1, 1},
	.Line         = {2, 2},
	.Thickness    = {1, 1},
}

// Contains geometry data a shader can run on
Mesh :: struct {
	vertex_ind_buffer: GL_BUF,
	vertex_pos_buffer: GL_BUF,
	index_count:       ℕ,
}

// Contains data required to run a GPU program on some mesh
Program :: struct {
	program: GL_PROGRAM,
	vao:     GL_VAO,
}

Program_Id :: enum {
	Rect,
	Circle,
	Line,
	Rounded_Line,
	Jfa,
	Jfa_Seed,
}
// }}}
// {{{ Meshes
create_mesh :: proc(vertices: []ℝ², indices: []u32) -> (out: Mesh) {
	out.index_count = len(indices)

	out.vertex_pos_buffer = gen_buffer()
	set_buffer(out.vertex_pos_buffer, vertices, usage = .Static)

	out.vertex_ind_buffer = gen_buffer()
	set_buffer(out.vertex_ind_buffer, indices, buffer = .Element_Array, usage = .Static)

	return out
}
// }}}
// {{{ Frame-buffers
FBO :: struct {
	fbo:               GL_FBO,
	tex_color:         GL_TEX,
	tex_depth_stencil: GL_TEX,
}

Framebuffer_ID :: enum {
	JFA_1,
	JFA_2,
}

// TODO: perhaps create/destroy these in bulk
create_framebuffer :: proc(dims: ℝ²) -> (out: FBO) {
	OpenGL.GenFramebuffers(1, &out.fbo)
	OpenGL.BindFramebuffer(OpenGL.FRAMEBUFFER, out.fbo)
	defer OpenGL.BindFramebuffer(OpenGL.FRAMEBUFFER, 0)

	OpenGL.GenTextures(1, &out.tex_color)
	OpenGL.BindTexture(OpenGL.TEXTURE_2D, out.tex_color)

	OpenGL.TexImage2D(
		OpenGL.TEXTURE_2D,
		0, // mipmap level of detail
		OpenGL.RGBA, // internal format
		i32(dims.x),
		i32(dims.y),
		0, // border: must be 0
		OpenGL.RGBA, // format
		OpenGL.UNSIGNED_BYTE, // pixel data type
		nil, // data pointer
	)

	OpenGL.TexParameteri(OpenGL.TEXTURE_2D, OpenGL.TEXTURE_MIN_FILTER, OpenGL.LINEAR)
	OpenGL.TexParameteri(OpenGL.TEXTURE_2D, OpenGL.TEXTURE_MAG_FILTER, OpenGL.LINEAR)
	OpenGL.FramebufferTexture2D(
		OpenGL.FRAMEBUFFER,
		OpenGL.COLOR_ATTACHMENT0,
		OpenGL.TEXTURE_2D,
		out.tex_color,
		0, // mipmap level of detail
	)

	// TODO: we do not sample those, so we should generate renderbuffers instead
	OpenGL.GenTextures(1, &out.tex_depth_stencil)
	OpenGL.BindTexture(OpenGL.TEXTURE_2D, out.tex_depth_stencil)

	OpenGL.TexImage2D(
		OpenGL.TEXTURE_2D,
		0, // mipmap level of detail
		OpenGL.DEPTH24_STENCIL8, // internal format
		i32(dims.x),
		i32(dims.y),
		0, // border: must be 0
		OpenGL.DEPTH_STENCIL, // format
		OpenGL.UNSIGNED_INT_24_8, // pixel data type
		nil, // data pointer
	)

	OpenGL.FramebufferTexture2D(
		OpenGL.FRAMEBUFFER,
		OpenGL.DEPTH_STENCIL,
		OpenGL.TEXTURE_2D,
		out.tex_depth_stencil,
		0, // mipmap level of detail
	)

	OpenGL.BindTexture(OpenGL.TEXTURE_2D, 0)

	log.assert(
		OpenGL.CheckFramebufferStatus(OpenGL.FRAMEBUFFER) == OpenGL.FRAMEBUFFER_COMPLETE,
		"Failed to initialize framebuffer",
	)

	return out
}

destroy_framebuffer :: proc(fbo: FBO) {
	textures: []GL_TEX = {fbo.tex_color, fbo.tex_depth_stencil}
	OpenGL.DeleteTextures(2, raw_data(textures))

	fbo := fbo.fbo
	OpenGL.DeleteFramebuffers(1, &fbo)
}
// }}}
// {{{ Shaders
@(private = "file")
Shader_Gen :: struct {
	template:       string,
	header:         string,
	toplevel_extra: string,
	main_extra:     string,
}

@(private = "file")
process_shader :: proc(gen: Shader_Gen) -> string {
	s := gen.template
	s, _ = strings.replace_all(s, "#header\n", gen.header, context.temp_allocator)
	s, _ = strings.replace_all(s, "#toplevelExtra\n", gen.toplevel_extra, context.temp_allocator)
	s, _ = strings.replace_all(s, "#mainExtra\n", gen.main_extra, context.temp_allocator)

	return s
}

// Parameter for instanced rendering, which gets passed along to the fragment 
// shader.
Instance_Param_Gen :: struct {
	name:      string,
	buf:       Instance_Param_Buf,
	vert_only: bool,
}

Program_Template :: enum {
	SDF,
	JFA,
	JFA_Seed,
}

Shader_Opts :: struct {
	params:   []Instance_Param_Gen,
	template: Program_Template,

	// SDF only parameters:
	// - The name of the top-level SDF function to be used for rendering
	sdf_name: string,

	// - Additional arguments to pass to the SDF function declared above. These 
	//   arguments get passed *before* the position vector
	sdf_args: []string,
}

gen_program_bin :: proc(
	id: Program_Id,
	instance_params: []Instance_Param_Gen = {},
) -> (
	out: Program,
	ok: bool,
) {
	@(static)
	@(rodata)
	VERT_TEMPLATE: [Program_Id]string = {
		.Rect         = "nope",
		.Line         = "nope",
		.Circle       = "nope",
		.Rounded_Line = "nope",
		.Jfa          = #load("../build/shaders/jfa.vert.glsl"),
		.Jfa_Seed     = #load("../build/shaders/jfa-seed.vert.glsl"),
	}

	@(static)
	@(rodata)
	FRAG_TEMPLATE: [Program_Id]string = {
		.Rect         = "nope",
		.Line         = "nope",
		.Circle       = "nope",
		.Rounded_Line = "nope",
		.Jfa          = #load("../build/shaders/jfa.frag.glsl"),
		.Jfa_Seed     = #load("../build/shaders/jfa-seed.frag.glsl"),
	}

	v_shader := VERT_TEMPLATE[id]
	f_shader := FRAG_TEMPLATE[id]

	out.program = OpenGL.load_shaders_source(v_shader, f_shader) or_return

	// {{{ Set up VAO
	// Create VAO
	OpenGL.GenVertexArrays(1, &out.vao)
	OpenGL.BindVertexArray(out.vao)
	defer OpenGL.BindVertexArray(0)

	for param in instance_params {
		buf_id := g_renderer_state().instance_buffers[param.buf]
		log.assertf(buf_id > 0, "Buffer %v not initialized", param.buf)
		bind_buffer(buf_id, .Array)

		shape := INSTANCE_PARAM_DIMS[param.buf]
		setup_vbo(
			buf_id,
			INSTANCE_PARAM_LOCATIONS[param.buf],
			rows = shape[0],
			cols = shape[1],
			divisor = 1,
		)
	}

	mesh := g_renderer_state().rect_mesh

	// Bind geometry data
	bind_buffer(mesh.vertex_pos_buffer, .Array)
	setup_vbo(mesh.vertex_pos_buffer, VERTEX_POS_LOCATION, ty = .Float, rows = 2)
	bind_buffer(mesh.vertex_ind_buffer, .Element_Array)
	// }}}

	return out, true
}

gen_program :: proc(opts: Shader_Opts) -> (out: Program, ok: bool) {
	// File ID to display in GLSL error messages
	@(static) next_shader_id := 0
	id := next_shader_id
	next_shader_id += 1

	@(static)
	@(rodata)
	VERT_TEMPLATE: [Program_Template]string = {
		.SDF      = #load("./shaders/sdf.vert.glsl"),
		.JFA      = "nope",
		.JFA_Seed = "nope",
	}

	@(static)
	@(rodata)
	FRAG_TEMPLATE: [Program_Template]string = {
		.SDF      = #load("./shaders/sdf.frag.glsl"),
		.JFA      = "nope",
		.JFA_Seed = "nope",
	}

	// Instance parameters passed to every shape
	@(static)
	@(rodata)
	COMMON_INSTANCE_PARAMS: [Program_Template][]Instance_Param_Gen = {
		.SDF      = {
			{buf = .Fill, name = "fill"},
			{buf = .Stroke, name = "stroke"},
			{buf = .Stroke_Width, name = "stroke_width"},
			{buf = .Z_Offset, name = "z_offset", vert_only = true},
			{buf = .Model_Mat, name = "model_matrix", vert_only = true},
		},
		.JFA      = {},
		.JFA_Seed = {},
	}

	opts := opts
	opts.params = slice.concatenate(
		[][]Instance_Param_Gen{COMMON_INSTANCE_PARAMS[opts.template], opts.params},
	)

	v_shader, f_shader: string

	// {{{  Vertex shader generation 
	{
		gen: Shader_Gen = {
			template = VERT_TEMPLATE[opts.template],
			header   = fmt.tprintfln("#version 430\n#line 1 %v", id),
		}

		toplevel_extra, main_extra: strings.Builder
		strings.builder_init_len_cap(
			&toplevel_extra,
			0,
			len(opts.params) * 64,
			context.temp_allocator,
		)
		strings.builder_init_len_cap(&main_extra, 0, len(opts.params) * 32, context.temp_allocator)

		for param in opts.params {
			fmt.sbprintfln(
				&toplevel_extra,
				"layout (location = %v) in %v i_%v;",
				INSTANCE_PARAM_LOCATIONS[param.buf],
				INSTANCE_PARAM_TYPE[param.buf],
				param.name,
			)
		}

		strings.write_rune(&toplevel_extra, '\n')

		for param in opts.params {
			if param.vert_only do continue
			fmt.sbprintfln(
				&toplevel_extra,
				"out %v v_%v;",
				INSTANCE_PARAM_TYPE[param.buf],
				param.name,
			)
		}

		gen.toplevel_extra = strings.to_string(toplevel_extra)

		for param in opts.params {
			if param.vert_only do continue
			fmt.sbprintfln(&main_extra, "  v_%v = i_%v;", param.name, param.name)
		}

		gen.main_extra = strings.to_string(main_extra)

		v_shader = process_shader(gen)
	}
	// }}}
	// {{{ Fragment shader generation
	{
		gen: Shader_Gen = {
			template = FRAG_TEMPLATE[opts.template],
			header   = fmt.tprintfln("#version 430\n#line 1 %v", id),
		}

		toplevel_extra: strings.Builder
		strings.builder_init_len_cap(
			&toplevel_extra,
			0,
			len(opts.params) * 32,
			context.temp_allocator,
		)

		for param in opts.params {
			if param.vert_only do continue
			fmt.sbprintfln(
				&toplevel_extra,
				"in %v v_%v;",
				INSTANCE_PARAM_TYPE[param.buf],
				param.name,
			)
		}

		if opts.template == .SDF {
			log.assert(len(opts.sdf_name) > 0, "Empty SDF function name")

			strings.write_rune(&toplevel_extra, '\n')
			fmt.sbprintln(&toplevel_extra, "float sdf(vec2 p) {")
			fmt.sbprintf(&toplevel_extra, "  return %v(", opts.sdf_name)
			for a in opts.sdf_args do fmt.sbprintf(&toplevel_extra, "%v, ", a)
			fmt.sbprintfln(&toplevel_extra, "p);")
			fmt.sbprintln(&toplevel_extra, "}")
		}

		gen.toplevel_extra = strings.to_string(toplevel_extra)

		f_shader = process_shader(gen)
	}
	// }}}

	out.program = OpenGL.load_shaders_source(v_shader, f_shader) or_return

	// {{{ Set up VAO
	// Create VAO
	OpenGL.GenVertexArrays(1, &out.vao)
	OpenGL.BindVertexArray(out.vao)
	defer OpenGL.BindVertexArray(0)

	for param in opts.params {
		buf_id := g_renderer_state().instance_buffers[param.buf]
		log.assertf(buf_id > 0, "Buffer %v not initialized", param.buf)
		bind_buffer(buf_id, .Array)

		shape := INSTANCE_PARAM_DIMS[param.buf]
		setup_vbo(
			buf_id,
			INSTANCE_PARAM_LOCATIONS[param.buf],
			rows = shape[0],
			cols = shape[1],
			divisor = 1,
		)
	}

	mesh := g_renderer_state().rect_mesh

	// Bind geometry data
	bind_buffer(mesh.vertex_pos_buffer, .Array)
	setup_vbo(mesh.vertex_pos_buffer, VERTEX_POS_LOCATION, ty = .Float, rows = 2)
	bind_buffer(mesh.vertex_ind_buffer, .Element_Array)
	// }}}

	return out, true
}

use_program :: proc(id: Program_Id) {
	program := g_renderer_state().programs[id]
	OpenGL.UseProgram(program.program)
	OpenGL.BindVertexArray(program.vao)
}
// }}}

// {{{ Render the entire queue
@(private = "file")
render_instanced :: proc(program_id: Program_Id, mesh: Mesh, shapes: ^[dynamic]Shape($T)) {
	use_program(program_id)

	state := g_renderer_state()
	steps := len(shapes) / INSTANCES

	for i := 0; i < len(shapes); i += INSTANCES {
		slice := shapes[i:]
		if len(slice) > INSTANCES {slice = slice[:INSTANCES]}

		for shape, i in slice {
			state.buf_matrices[i] = to_transform(shape)
			state.buf_floats[i] = shape.z
			state.buf_colors[i] = shape.fill
		}

		set_buffer(state.instance_buffers[.Model_Mat], &state.buf_matrices)
		set_buffer(state.instance_buffers[.Fill], &state.buf_colors)
		set_buffer(state.instance_buffers[.Z_Offset], &state.buf_floats)

		for shape, i in slice {
			state.buf_colors[i] = shape.stroke
			state.buf_floats[i] = shape.stroke_width
		}

		set_buffer(state.instance_buffers[.Stroke], &state.buf_colors)
		set_buffer(state.instance_buffers[.Stroke_Width], &state.buf_floats)

		when T == AABB {
			for shape, i in slice do state.buf_vecs[i] = aabb_center(shape.data)
			set_buffer(state.instance_buffers[.Center], &state.buf_vecs)

			for shape, i in slice do state.buf_vecs[i] = shape.data.dimensions
			set_buffer(state.instance_buffers[.Dimensions], &state.buf_vecs)
		}

		when T == Circle2 {
			for shape, i in slice do state.buf_vecs[i] = shape.data.center
			set_buffer(state.instance_buffers[.Center], &state.buf_vecs)

			for shape, i in slice do state.buf_floats[i] = shape.data.radius
			set_buffer(state.instance_buffers[.Radius], &state.buf_floats)
		}

		when T == Rounded_Line || T == Line {
			for shape, i in slice {
				state.buf_lines[i][0] = shape.from
				state.buf_lines[i][1] = shape.to
				state.buf_floats[i] = shape.thickness
			}

			set_buffer(state.instance_buffers[.Line], &state.buf_lines)
			set_buffer(state.instance_buffers[.Thickness], &state.buf_floats)
		}

		OpenGL.DrawElementsInstanced(
			OpenGL.TRIANGLE_FAN,
			i32(mesh.index_count),
			OpenGL.UNSIGNED_INT,
			nil,
			i32(len(slice)),
		)
	}

	clear(shapes)
}

render_queue :: proc() {
	state := g_renderer_state()

	// Update uniform data
	set_buffer(state.ubos[.Globals], &state.globals, buffer = .Uniform)

	// Toggle the wireframe
	OpenGL.PolygonMode(OpenGL.FRONT_AND_BACK, state.wireframe ? OpenGL.LINE : OpenGL.FILL)

	render_instanced(.Rect, state.rect_mesh, &state.q_rects)
	render_instanced(.Circle, state.rect_mesh, &state.q_circles)
	render_instanced(.Line, state.rect_mesh, &state.q_lines)
	render_instanced(.Rounded_Line, state.rect_mesh, &state.q_rounded_lines)

	OpenGL.UseProgram(0)
}
// }}}
// {{{ JFA
INPUT_TEXTURE_LOCATION :: 0

jfa :: proc() {
	state := g_renderer_state()
	dims := screen_dimensions()

	jfa1 := &state.framebuffers[.JFA_1]
	jfa2 := &state.framebuffers[.JFA_2]

	OpenGL.BindFramebuffer(OpenGL.FRAMEBUFFER, state.pass == .SDF ? 0 : jfa1.fbo)
	clear_screen()
	render_queue()
	if state.pass == .SDF do return

	OpenGL.BindFramebuffer(OpenGL.FRAMEBUFFER, state.pass == .JFA_Seed ? 0 : jfa2.fbo)
	clear_screen()

	use_program(.Jfa_Seed)
	OpenGL.ActiveTexture(OpenGL.TEXTURE0)
	OpenGL.BindTexture(OpenGL.TEXTURE_2D, jfa1.tex_color)
	OpenGL.Uniform1i(INPUT_TEXTURE_LOCATION, 0)
	OpenGL.DrawElements(
		OpenGL.TRIANGLE_FAN,
		i32(state.rect_mesh.index_count),
		OpenGL.UNSIGNED_INT,
		nil,
	)
	if state.pass == .JFA_Seed do return

	use_program(.Jfa)
	OpenGL.ActiveTexture(OpenGL.TEXTURE0)
	OpenGL.Uniform1i(INPUT_TEXTURE_LOCATION, 0)

	mat22: matrix[2, 2]f32
	OpenGL.Uniform2fv(INPUT_TEXTURE_LOCATION, 2, transmute([^]f32)&mat22)
	OpenGL.UniformMatrix2x4fv(INPUT_TEXTURE_LOCATION, 2, transmute([^]f32)&mat22)

	input_fbo := jfa2
	output_fbo := jfa1

	uniforms: JFA_Uniforms = {
		resolution = dims,
	}

	passes := math.ceil(math.log2(math.max(dims.x, dims.y)))

	for i in 0 ..< passes {
		OpenGL.BindFramebuffer(
			OpenGL.FRAMEBUFFER,
			state.pass == .JFA && i == passes - 1 ? 0 : output_fbo.fbo,
		)
		clear_screen()

		uniforms.offset = math.pow(2, passes - i - 1)
		set_buffer(state.ubos[.JFA], &uniforms, buffer = .Uniform)

		OpenGL.BindTexture(OpenGL.TEXTURE_2D, input_fbo.tex_color)
		OpenGL.DrawElements(
			OpenGL.TRIANGLE_FAN,
			i32(state.rect_mesh.index_count),
			OpenGL.UNSIGNED_INT,
			nil,
		)

		temp := input_fbo
		input_fbo = output_fbo
		output_fbo = temp
	}
}
// }}}
