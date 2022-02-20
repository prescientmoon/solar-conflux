#version 300 es

in vec4 a_position;

// TODO: use an UBO perhaps?
uniform mat4 u_transform_matrix;
uniform mat4 u_projection_matrix;
uniform mat4 u_texture_matrix;

out vec2 v_texcoord;

void main() {
    gl_Position = u_transform_matrix * u_projection_matrix * a_position;
    v_texcoord = (u_texture_matrix * a_texcoord).xy;
}