in vec2 v_uv;

void vert() {
  gl_Position = vec4(v_uv * 2 - 1, 0, 1);
}

// uniform sampler2D input_texture;
uniform vec2 input_texture;
out vec4 frag_color;

void frag() {
  vec4 sample_value = texture(input_texture, v_uv);
  frag_color = vec4(v_uv * sample_value.a, 0, 1);
}
