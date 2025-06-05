#header

out vec4 FragColor;
in vec2 v_uv;

uniform sampler2D input_texture;
layout(std140, binding = 1) uniform Jfa {
  float u_offset;
  vec2 u_resolution;
};

void main() {
  vec4 nearest_seed = vec4(-2.0);
  float nearest_dist = 999999999.9;

  for (float y = -1.0; y <= 1.0; y += 1.0) {
    for (float x = -1.0; x <= 1.0; x += 1.0) {
      vec2 sampleUV = v_uv + u_offset * vec2(x, y) / u_resolution;

      if (sampleUV.x < 0.0 || sampleUV.x > 1.0 || sampleUV.y < 0.0 || sampleUV.y > 1.0) {
        continue;
      }

      vec4 sample_value = texture(input_texture, sampleUV);

      if (sample_value.xy != vec2(0.0)) {
        vec2 diff = (sample_value.xy - v_uv) * u_resolution;
        float dist = dot(diff, diff);

        if (dist < nearest_dist) {
          nearest_dist = dist;
          nearest_seed = sample_value;
        }
      }
    }
  }

  FragColor = nearest_seed;
}
