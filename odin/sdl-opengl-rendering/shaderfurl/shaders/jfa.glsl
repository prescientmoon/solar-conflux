#include <utils>

in vec2 v_uv;

void vert() {
  gl_Position = vec4(v_uv * 2 - 1, 0, 1);
}

uniform struct JfaInputs {
  float u_offset;
  vec2 u_resolution;
} inputs;

// uniform sampler2D input_texture;
uniform vec2 input_texture;
out vec4 frag_color;

void frag() {
  vec4 nearest_seed = vec4(-2.0);
  float nearest_dist = 999999999.9; // squared distance in pixel space

  for (float y = -1.0; y <= 1.0; y += 1.0) {
    for (float x = -1.0; x <= 1.0; x += 1.0) {
      vec2 sample_uv = v_uv +
          inputs.u_offset * vec2(x, y) / inputs.u_resolution;

      if (!uv_in_bounds(sample_uv)) continue;

      vec4 sample_value = texture(input_texture, sample_uv);

      if (sample_value.xy != vec2(0.0)) {
        vec2 diff = (sample_value.xy - v_uv) * inputs.u_resolution;
        float dist = dot(diff, diff);

        if (dist < nearest_dist) {
          nearest_dist = dist;
          nearest_seed = sample_value;
        }
      }
    }
  }

  frag_color = nearest_seed;
}
