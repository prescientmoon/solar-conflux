package shaders

import "core:log"
import "core:math/linalg/glsl"
import "vendor:OpenGL"

// ========== UBOs
Ubo_Id :: enum {
  Globals,
}

@(rodata)
UBO_BINDINGS : [Ubo_Id]u32 = {
  .Globals = 0,
}

U_Globals :: struct {
  u_viewport_matrix : glsl.mat4,
  u_aa_width : f32,
}

// ========== File: shape
Shape_U_U_Viewport_Matrix :: glsl.mat4
Shape_U_Globals :: struct {
u_viewport_matrix : 
glsl.mat4,
u_aa_width : 
f32,
}


// ========== File: rect
Rect_U_Input_Textures :: [3]glsl.mat2x3
Rect_A_V_Uv :: glsl.vec2
Rect_A_V_Things :: [4]struct {
foo : 
glsl.vec2,
bar : 
glsl.mat3x2,
}

RECT_UNIFORM_ID :: enum {
  U_Viewport_Matrix,
  Globals,
  Input_Textures,
}

@(rodata)
Rect_Uniform_Location_Table : [RECT_UNIFORM_ID]u32 = {
  .U_Viewport_Matrix = 0,
  .Globals = 4,
  .Input_Textures = 9,
}

rect_set_u_viewport_matrix :: proc(value : ^Shape_U_U_Viewport_Matrix) {
OpenGL.UniformMatrix4fv(0, 1, false, auto_cast value)
}

rect_set_globals :: proc(value : ^Shape_U_Globals) {
OpenGL.UniformMatrix4fv(4, 1, false, auto_cast &value.u_viewport_matrix)
OpenGL.Uniform1f(8, value.u_aa_width)
}

rect_set_input_textures :: proc(value : ^Rect_U_Input_Textures) {
for &value, i in value {
loc := 9 + 2 * i
OpenGL.UniformMatrix2x3fv(loc, 1, false, auto_cast value)
}
}

RECT_ATTRIB_ID :: enum {
  V_Uv,
  V_Things,
}

@(rodata)
RECT_ATTRIB_LOCATION_TABLE : [RECT_ATTRIB_ID]u32 = {
  .V_Uv = 0,
  .V_Things = 1,
}

 rect_create_program :: proc(buffers: [RECT_ATTRIB_ID]u32, indices: u32 = 0, instanced : bit_set[RECT_ATTRIB_ID] = {}) -> (program: u32, vao: u32) {
        v_shader: string // TODO
        f_shader: string // TODO

        program_ok: bool
        program, program_ok = OpenGL.load_shaders_source(v_shader, f_shader) 
        if !program_ok do log.panic("Failed to create program `rect`")
      
// Create VAO
OpenGL.GenVertexArrays(1, &vao)
OpenGL.BindVertexArray(vao)
defer OpenGL.BindVertexArray(0)

// Set up .V_Uv
OpenGL.BindBuffer(buffers[.V_Uv], OpenGL.ARRAY_BUFFER)
OpenGL.EnableVertexAttribArray(0)
OpenGL.VertexAttribPointer(
        0,
        2,
        OpenGL.FLOAT,
        false,
        10,
        uintptr(0),
      )
      if V_Uv in instanced do OpenGL.VertexAttribDivisor(0, 1)

// Set up .V_Things
OpenGL.BindBuffer(buffers[.V_Things], OpenGL.ARRAY_BUFFER)
for i in i32(0)..<count {
loc := 1 + 4 * i
offset := 0 + 40 * i
OpenGL.EnableVertexAttribArray(loc)
OpenGL.VertexAttribPointer(
        loc,
        2,
        OpenGL.FLOAT,
        false,
        160,
        uintptr(offset),
      )
      if V_Things in instanced do OpenGL.VertexAttribDivisor(loc, 1)
for i in i32(0)..<3
loc := loc + 1 + i
offset := offset + 10 + 10 * i
OpenGL.EnableVertexAttribArray(loc)
OpenGL.VertexAttribPointer(
        loc,
        2,
        OpenGL.FLOAT,
        false,
        160,
        uintptr(offset),
      )
      if V_Things in instanced do OpenGL.VertexAttribDivisor(loc, 1)
}
}

return program, vao
}


