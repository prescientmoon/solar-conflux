#version 330

out vec4 FragColor;

in vec4 vertexColor;
in vec2 vertexPos;

float sdfLine(vec2 p) {
  return abs(p.y) - 0.5;
}

void main() {
  float aWidth = 0.001;
  float dist = sdfLine(vertexPos);
  float alpha = smoothstep(aWidth, -aWidth, dist);

  FragColor = vec4(vertexColor.xyz, alpha * vertexColor.a);
}
