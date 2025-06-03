#version 330

out vec4 FragColor;

in vec4 vertexColor;
in vec2 vertexPos;

layout(std140) uniform Globals {
  mat4 viewportMatrix;
  float aaWidth;
};

float sdfCircle(vec2 p) {
  return p.x * p.x + p.y * p.y - 1;
}

void main() {
  float dist = sdfCircle(vertexPos);
  float alpha = smoothstep(aaWidth, -aaWidth, dist);

  if (alpha < 0.001) discard;
  FragColor = vec4(vertexColor.xyz, alpha * vertexColor.a);
}
