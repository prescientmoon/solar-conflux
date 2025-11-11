#include <shape>

uniform mat2x3 input_textures[3];

in vec2 v_uv;
in struct {
  vec2 foo;
  mat3x2 bar;
} v_things[4];
out vec4 frag_color;

void vert() {
  gl_Position = vec4(v_uv * 2 - 1, 0, 1);
}

void frag() {
  vec4 sample_value = texture(input_texture, v_uv);
  frag_color = vec4(v_uv * sample_value.a, 0, 1);
}
