package shaders

// ========== UBOs
U_Globals :: struct {
	u_viewport_matrix: matrix[4, 4]f32,
	u_aa_width:        f32,
}

UBO_ID :: enum {
	Globals,
}

// ========== Files: common
Shape_Opts :: struct {
	i_fill:         [4]f32,
	i_stroke:       [4]f32,
	i_stroke_width: f32,
	i_z_offset:     f32,
	i_model_matrix: matrix[2, 3]f32,
}

// ========== Programs: jfa-seed
Jfa_Seed_Input :: enum {
	V_Uv,
}

Jfa_Seed_Uniform :: enum {
	Input_Texture,
	Jfa_Inputs,
}

@(rodata)
JFA_SEED_INPUT_LOCATION_TABLE: [Jfa_Seed_Input]u32 = {
	.V_Uv = 0,
}

@(rodata)
JFA_SEED_UNIFORM_LOCATION_TABLE: [Jfa_Seed_Uniform]u32 = {
	.Input_Texture = 0,
	.Jfa_Inputs    = 1,
}

programs›jfa_seed›create_program :: proc(
	buffers: [Jfa_Seed_Input]u32,
	instanced: bit_set[Jfa_Seed_Input] = {},
) {
}

// =========== Programs: jfa
Jfa_Input :: enum {
	V_Uv,
}

Jfa_Uniform :: enum {
	Input_Texture,
}

@(rodata)
JFA_INPUT_LOCATION_TABLE: [Jfa_Input]u32 = {
	.V_Uv = 0,
}

@(rodata)
JFA_UNIFORM_LOCATION_TABLE: [Jfa_Uniform]u32 = {
	.Input_Texture = 0,
}

programs›jfa›create_program :: proc(
	buffers: [Jfa_Input]u32,
	instanced: bit_set[Jfa_Input] = {},
) {
	// ...
}

programs›jfa›set_u_input_texture :: proc() {
	// ...
}

// ========== Programs: rect
Rect_Input :: enum {
	A_Pos,
	I_Center,
	I_Dimensions,
	Shape_Opts,
}

@(rodata)
RECT_INPUT_LOCATION_TABLE: [Rect_Input]u32 = {
	.A_Pos        = 0,
	.I_Center     = 1,
	.I_Dimensions = 2,
	.Shape_Opts   = 3,
}
