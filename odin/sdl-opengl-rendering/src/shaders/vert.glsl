#version 430

layout (location = 0) in vec2 aPos;
layout (location = 1) in vec4 instanceFill;
layout (location = 2) in mat3 instanceMatrix;

out vec4 vertexColor;
out vec2 vertexPos;

layout(std140, binding = 0) uniform Globals {
  mat4 viewportMatrix;
  float aaWidth; // Anti aliasing width
};

void main() {
  vec4 pos    = viewportMatrix * vec4(instanceMatrix * vec3(aPos.xy, 1), 1);
  // vec3 pos    = vec3(aPos.xy, 1);
  gl_Position = vec4(pos.xyz, 1);
  vertexColor = instanceFill;
  vertexPos = aPos.xy;
}
