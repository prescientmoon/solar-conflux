#version 330

layout (location = 0) in vec2 aPos;
layout (location = 1) in vec4 instanceFill;
layout (location = 2) in mat3 instanceMatrix;

out vec4 vertexColor;
out vec2 vertexPos;

void main() {
  vec3 pos    = instanceMatrix * vec3(aPos.xy, 1);
  gl_Position = vec4(pos.xyz, 1);
  vertexColor = instanceFill;
  vertexPos = aPos.xy;
}
