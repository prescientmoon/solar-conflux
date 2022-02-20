#version 300 es

in vec3 a_position;

// TODO: use an UBO perhaps?
uniform mat3 u_transform_matrix;
uniform mat3 u_projection_matrix;
uniform mat3 u_texture_matrix;
uniform mat3 u_world_matrix;
uniform float u_layer;

out vec2 v_texcoord;

void main() {
    vec3 position = u_projection_matrix * u_world_matrix * u_transform_matrix * vec3(a_position.xy, 1);
    gl_Position = vec4(position.xy, u_layer, 1.0);
    // gl_Position = vec4(a_position.xy, u_layer, 1);
    v_texcoord = (u_texture_matrix * vec3(a_position.xy, 1)).xy;
}