#header

layout (location = 0) in vec2 a_pos;
out vec2 v_pos;

layout(std140, binding = 0) uniform Globals {
  mat4 u_viewport_matrix;
  float u_aa_width; // Anti aliasing width
};

#toplevelExtra

void main() {
  vec4 pos    = u_viewport_matrix * vec4(i_model_matrix * vec3(a_pos, 1), 1);
  gl_Position = vec4(pos.xyz, 1);

  v_pos = (i_model_matrix * vec3(a_pos, 1)).xy;

  #mainExtra
}
