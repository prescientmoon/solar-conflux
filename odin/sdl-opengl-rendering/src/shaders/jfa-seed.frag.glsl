#header

out vec4 FragColor;
in vec2 v_uv;

uniform sampler2D input_texture;
layout(std140, binding = 1) uniform Jfa {
  float u_offset;
  vec2 u_resolution;
};

void main() {
  vec4 sample_value = texture(input_texture, v_uv);

  FragColor = vec4(v_uv * sample_value.a, 0, 1);
}
