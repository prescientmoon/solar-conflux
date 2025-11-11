#include <globals>

in vec2 a_pos;
varying vec2 v_pos;

in struct {
  vec4 fill;
  vec4 stroke;
  float stroke_width;
  float z_offset;
  mat3x2 model_matrix;
} i_opts;

void compute_shape_pos() {
  v_pos = i_opts.model_matrix * vec3(a_pos, 1);
  vec4 pos = u_viewport_matrix * vec4(v_pos, i_opts.z_offset, 1);
  gl_Position = vec4(pos.xyz, 1);
}

vec4 shape_color(float dist) {
  float alpha = smoothstep(u_aa_width, -u_aa_width, dist);
  float s_alpha = smoothstep(u_aa_width, -u_aa_width, abs(dist) - i_opts.stroke_width);

  if (alpha < 0.001 && s_alpha < 0.001) discard;

  vec4 result = vec4(i_opts.fill.xyz, alpha * i_opts.fill.a);
  result = s_alpha * i_opts.stroke + (1 - s_alpha) * result;

  return result;
}
