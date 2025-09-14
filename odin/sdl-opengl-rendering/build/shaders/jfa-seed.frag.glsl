#version 430
#define FRAG
#line 1 7 // "jfa-seed.glsl"
#line 1 7 // "common.glsl"
#define IN(loc, type, name) layout(location = loc) in type name;
#define OUT(loc, type, name) layout(location = loc) out type name;

#ifdef VERT
#define VIN(loc, type, name) IN(loc, type, name);
#define VARYING(loc, type, name) OUT(loc, type, name);
#define FOUT(loc, type, name)
#endif

#ifdef FRAG
#define VIN(loc, type, name)
#define VARYING(loc, type, name) IN(loc, type, name);
#define FOUT(loc, type, name) OUT(loc, type, name);
#endif
#line 2 7 // "jfa-seed.glsl"

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