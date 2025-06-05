#header

out vec4 FragColor;
in vec2 v_pos;

layout(std140, binding = 0) uniform Globals {
  mat4 u_viewport_matrix;
  float u_aa_width;
};

float sdf_rect(vec2 center, vec2 dims, vec2 p) {
  vec2 d = abs(p - center) - dims / 2;
  return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0);
}

float sdf_circle(vec2 center, float radius, vec2 p) {
  return length(p - center) - radius;
}

float sdf_line(vec2[2] line, float thickness, vec2 p) {
  return 100.0;
}

float sdf_rounded_line(vec2[2] line, float thickness, vec2 p) {
  vec2 pa = p - line[0];
  vec2 ba = line[1] - line[0];

  float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
  return length(pa - ba * h) - thickness;
}

#toplevelExtra

void main() {
  // This function gets auto-generated to call the right sdf
  float dist = sdf(v_pos);
  float alpha = smoothstep(u_aa_width, -u_aa_width, dist);
  float s_alpha = smoothstep(u_aa_width, -u_aa_width, abs(dist) - v_stroke_width);

  if (alpha < 0.001 && s_alpha < 0.001) discard;

  FragColor = vec4(v_fill.xyz, alpha * v_fill.a);
  FragColor = s_alpha * v_stroke + (1 - s_alpha) * FragColor;
}
