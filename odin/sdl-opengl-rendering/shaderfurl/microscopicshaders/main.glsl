uniform struct Light {
  vec2 center;
} lights[16];

uniform mat2x2 transform;

varying vec2 favourite_light;
varying vec2 least_favourite_light;

in vec2 v_uv;
out vec4 frag_color;

float sdf_rect(vec2 center, vec2 dims, vec2 p) {
  vec2 d = abs(p - center) - dims / 2;
  return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0);
}
