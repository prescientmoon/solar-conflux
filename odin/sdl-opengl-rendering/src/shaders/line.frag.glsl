#version 330

out vec4 FragColor;

in vec4 vFill;
in vec2 vPos;

layout(std140) uniform Globals {
  mat4 viewportMatrix;
  float aaWidth;
};

float sdfLine(vec2 p) {
  return abs(p.y) - 0.5;
}

void main() {
  float dist = sdfLine(vPos);
  float alpha = smoothstep(aaWidth, -aaWidth, dist);

  if (alpha < 0.001) discard;
  FragColor = vec4(vFill.xyz, alpha * vFill.a);
}
