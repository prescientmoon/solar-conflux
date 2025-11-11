bool uv_in_bounds(vec2 uv) {
  return uv.x >= 0.0 && uv.x <= 1.0
    && uv.y >= 0.0 && uv.y <= 1.0;
}

float sdf_rect(vec2 center, vec2 dims, vec2 p) {
  vec2 d = abs(p - center) - dims / 2;
  return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0);
}
