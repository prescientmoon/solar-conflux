#header

layout(location = 0) in vec2 a_pos;
out vec2 v_uv;

layout(std140, binding = 1) uniform Jfa {
  float u_offset;
  vec2 u_resolution;
};

void main() {
  v_uv = a_pos;
  gl_Position = vec4(a_pos * 2 - 1, 0, 1);
}
