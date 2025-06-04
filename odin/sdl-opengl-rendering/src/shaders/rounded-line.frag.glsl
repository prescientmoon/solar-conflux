#version 330

out vec4 FragColor;

in vec4  vFill;
in vec4  vStroke;
in float vStrokeThickness;
in vec2  vPos;
in vec2  vFrom;
in vec2  vTo;
in float vThickness;

layout(std140) uniform Globals {
  mat4 viewportMatrix;
  float aaWidth;
};

float sdfRoundedLine(vec2 p) {
  vec2 a  = vFrom;
  vec2 b  = vTo;
  vec2 pa = p-a;
  vec2 ba = b-a;

  float h = clamp(dot(pa,ba)/dot(ba, ba), 0.0, 1.0);
  return length(pa - ba * h) - vThickness;
}

void main() {
  float dist    = sdfRoundedLine(vPos);
  float alpha   = smoothstep(aaWidth, -aaWidth, dist);
  float s_alpha = smoothstep(aaWidth, -aaWidth, abs(dist) - vStrokeThickness);

  if (alpha < 0.001 && s_alpha < 0.001) discard;
  FragColor = vec4(vFill.xyz, alpha * vFill.a);
  FragColor = s_alpha * vStroke + (1 - s_alpha) * FragColor;
}
