#version 300 es

in vec3 a_position;

uniform mat3 u_transform_matrix;
uniform mat3 u_projection_matrix;
uniform mat3 u_world_matrix;

out vec2 v_coord;

void main() {
    vec3 position = u_projection_matrix * u_world_matrix * u_transform_matrix * vec3(a_position.xy, 1);

    gl_Position = vec4(position.xyz, 1.0);
    v_coord = a_position.xy;
}