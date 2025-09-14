#include "common"

layout(location = 0) uniform sampler2D input_texture;

VARYING(0, vec2, v_uv)
VIN(0, vec2, a_pos)
FOUT(0, vec4, FragColor)

#ifdef VERT
void main() {
  v_uv = a_pos;
  gl_Position = vec4(a_pos * 2 - 1, 0, 1);
}
#endif
#ifdef FRAG
void main() {
  vec4 sample_value = texture(input_texture, v_uv);

  FragColor = vec4(v_uv * sample_value.a, 0, 1);
}
#endif
