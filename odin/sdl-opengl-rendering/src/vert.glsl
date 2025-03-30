#version 330

in vec2 aPos;

out vec4 vertexColor;

void main() {
  gl_Position = vec4(aPos.x, aPos.y, 0, 1);
  vertexColor = vec4((aPos.x + 1) / 2, (aPos.y + 1) / 2, 1, 1);
}
