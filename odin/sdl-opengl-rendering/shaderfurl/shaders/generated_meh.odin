package shaders

import "core:log"
import "core:math/linalg/glsl"
import "vendor:OpenGL"

// ========== UBOs
Ubo_Id :: enum {
	Globals,
}

@(rodata)
UBO_BINDINGS: [Ubo_Id]u32 = {
	.Globals = 0,
}

U_Globals :: struct {
	u_viewport_matrix: glsl.mat4,
	u_aa_width:        f32,
}

// ========== File: globals


// ========== File: rect
Rect_I_Center_Type :: glsl.vec2
Rect_I_Dimensions_Type :: glsl.vec2

RECT_ATTRIB_ID :: enum {
	I_Center,
	I_Dimensions,
	A_Pos,
	I_Opts,
}

@(rodata)
Rect_Attrib_Location_Table: [RECT_ATTRIB_ID]u32 = {
	.I_Center     = 0,
	.I_Dimensions = 1,
	.A_Pos        = 2,
	.I_Opts       = 3,
}

rect_create_program :: proc(
	buffers: [RECT_ATTRIB_ID]u32,
	indices: u32 = 0,
	instanced: bit_set[RECT_ATTRIB_ID] = {},
) -> (
	program: u32,
	vao: u32,
) {
	v_shader: string // TODO
	f_shader: string // TODO
	program =
		OpenGL.load_shaders_source(v_shader, f_shader) or_else log.panic(
			"Failed to create program `rect`",
		)

	// Create VAO
	OpenGL.GenVertexArrays(1, &vao)
	OpenGL.BindVertexArray(vao)
	defer OpenGL.BindVertexArray(0)

	// Set up .I_Center
	OpenGL.BindBuffer(buffers[.I_Center], OpenGL.ARRAY_BUFFER)

	// Set up .I_Dimensions
	OpenGL.BindBuffer(buffers[.I_Dimensions], OpenGL.ARRAY_BUFFER)

	// Set up .A_Pos
	OpenGL.BindBuffer(buffers[.A_Pos], OpenGL.ARRAY_BUFFER)

	// Set up .I_Opts
	OpenGL.BindBuffer(buffers[.I_Opts], OpenGL.ARRAY_BUFFER)

	return program, vao
}


// ========== File: jfa
Jfa_V_Uv_Type :: glsl.vec2

JFA_ATTRIB_ID :: enum {
	V_Uv,
}

@(rodata)
Jfa_Attrib_Location_Table: [JFA_ATTRIB_ID]u32 = {
	.V_Uv = 0,
}

jfa_create_program :: proc(
	buffers: [JFA_ATTRIB_ID]u32,
	indices: u32 = 0,
	instanced: bit_set[JFA_ATTRIB_ID] = {},
) -> (
	program: u32,
	vao: u32,
) {
	v_shader: string // TODO
	f_shader: string // TODO
	program =
		OpenGL.load_shaders_source(v_shader, f_shader) or_else log.panic(
			"Failed to create program `jfa`",
		)

	// Create VAO
	OpenGL.GenVertexArrays(1, &vao)
	OpenGL.BindVertexArray(vao)
	defer OpenGL.BindVertexArray(0)

	// Set up .V_Uv
	OpenGL.BindBuffer(buffers[.V_Uv], OpenGL.ARRAY_BUFFER)

	return program, vao
}

JFA_UNIFORM_ID :: enum {
	Inputs,
	Input_Texture,
}

@(rodata)
Jfa_Uniform_Location_Table: [JFA_UNIFORM_ID]u32 = {
	.Inputs        = 0,
	.Input_Texture = 2,
}

jfa_set_inputs :: proc(value: ^struct {
		u_offset:     f32,
		u_resolution: glsl.vec2,
	}) {
	loc: i32 = 0
	{
		value := &value.u_offset
		OpenGL.Uniform1f(loc, value^)
	}
	{
		loc := loc + 1
		value := &value.u_resolution
		OpenGL.Uniform2fv(loc, 1, auto_cast value)
	}
}

jfa_set_input_texture :: proc(value: ^glsl.vec2) {
	loc: i32 = 2
	OpenGL.Uniform2fv(loc, 1, auto_cast value)
}


// ========== File: jfa-seed
Jfa_Seed_V_Uv_Type :: glsl.vec2

JFA_SEED_ATTRIB_ID :: enum {
	V_Uv,
}

@(rodata)
Jfa_Seed_Attrib_Location_Table: [JFA_SEED_ATTRIB_ID]u32 = {
	.V_Uv = 0,
}

jfa_seed_create_program :: proc(
	buffers: [JFA_SEED_ATTRIB_ID]u32,
	indices: u32 = 0,
	instanced: bit_set[JFA_SEED_ATTRIB_ID] = {},
) -> (
	program: u32,
	vao: u32,
) {
	v_shader: string // TODO
	f_shader: string // TODO
	program =
		OpenGL.load_shaders_source(v_shader, f_shader) or_else log.panic(
			"Failed to create program `jfa-seed`",
		)

	// Create VAO
	OpenGL.GenVertexArrays(1, &vao)
	OpenGL.BindVertexArray(vao)
	defer OpenGL.BindVertexArray(0)

	// Set up .V_Uv
	OpenGL.BindBuffer(buffers[.V_Uv], OpenGL.ARRAY_BUFFER)

	return program, vao
}

JFA_SEED_UNIFORM_ID :: enum {
	Input_Texture,
}

@(rodata)
Jfa_Seed_Uniform_Location_Table: [JFA_SEED_UNIFORM_ID]u32 = {
	.Input_Texture = 0,
}

jfa_seed_set_input_texture :: proc(value: ^glsl.vec2) {
	loc: i32 = 0
	OpenGL.Uniform2fv(loc, 1, auto_cast value)
}


// ========== File: shape
Shape_A_Pos_Type :: glsl.vec2
Shape_I_Opts_Type :: struct {
	fill:         glsl.vec4,
	stroke:       glsl.vec4,
	stroke_width: f32,
	z_offset:     f32,
	model_matrix: glsl.mat3x2,
}

set_i_opts_buffer :: proc(buffer: u32, i_opts: []Shape_I_Opts_Type) {}
