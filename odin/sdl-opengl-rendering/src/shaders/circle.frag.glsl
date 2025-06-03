#version 330

out vec4 FragColor;

in vec4 vertexColor;
in vec2 vertexPos;

void main() {
  vec2 p = vertexPos;
  float mask = p.x * p.x + p.y * p.y > 1 ? 0 : 1;

  FragColor = mask * vertexColor;
}
