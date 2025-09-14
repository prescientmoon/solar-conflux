#version 430
#define FRAG
#line 1 // "rect.glsl"
#line 1 // "common.glsl"
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
#line 2 // "rect.glsl"
#line 1 // "shape.glsl"
#line 2 // "shape.glsl"
#line 1 // "globals.glsl"
layout(std140, binding = 0) uniform Globals {
  mat4 u_viewport_matrix;
  float u_aa_width; // Anti aliasing width
};
#line 3 // "shape.glsl"

VIN(0, vec2, a_pos)
VIN(1, vec4, i_fill)
VIN(2, vec4, i_stroke)
VIN(3, float, i_stroke_width)
VIN(4, float, i_z_offset)
VIN(5, mat3x2, i_model_matrix)

VARYING(0, vec2, v_pos)
VARYING(1, vec4, v_fill)
VARYING(2, vec4, v_stroke)
VARYING(3, vec4, v_stroke_width)

void pass_shape_params() {
  v_pos = i_model_matrix * vec3(a_pos, 1);
  vec4 pos = u_viewport_matrix * vec4(v_pos, i_z_offset, 1);
  gl_Position = vec4(pos.xyz, 1);

  v_fill = i_fill;
  v_stroke = i_stroke;
  v_stroke_width = i_stroke_width;
}

vec4 shape_color(float dist) {
  float alpha = smoothstep(u_aa_width, -u_aa_width, dist);
  float s_alpha = smoothstep(u_aa_width, -u_aa_width, abs(dist) - v_stroke_width);

  if (alpha < 0.001 && s_alpha < 0.001) discard;

  vec4 result = vec4(v_fill.xyz, alpha * v_fill.a);
  result = s_alpha * v_stroke + (1 - s_alpha) * result;

  return result
}
#line 3 // "rect.glsl"

VIN(8, vec2, i_center)
VARYING(8, vec2, v_center)

VIN(9, vec2, i_dimensions)
VARYING(9, vec2, v_dimensions)

float sdf_rect(vec2 center, vec2 dims, vec2 p) {
  vec2 d = abs(p - center) - dims / 2;
  return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0);
}

#ifdef VERT
void main() {
  pass_shape_params();
  v_center = i_center;
  v_dimensions = i_dimensions;
}
#endif
#ifdef FRAG
void main() {
  FragColor = shape_color(sdf_rect(v_center, v_dimensions, v_pos));
}
#endif