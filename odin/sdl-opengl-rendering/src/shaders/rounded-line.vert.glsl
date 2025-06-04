#version 430

layout (location = 0) in vec2  aPos;
layout (location = 1) in vec4  instanceFill;
layout (location = 2) in mat3  instanceMatrix;
layout (location = 5) in vec2  instanceFrom;
layout (location = 6) in vec2  instanceTo;
layout (location = 7) in float instanceThickness;

out vec4  vertexColor;
out vec2  vertexPos;
out vec2  vertexFrom;
out vec2  vertexTo;
out float vertexThickness;

layout(std140, binding = 0) uniform Globals {
  mat4 viewportMatrix;
  float aaWidth; // Anti aliasing width
};

void main() {
  vec4 pos    = viewportMatrix * vec4(instanceMatrix * vec3(aPos, 1), 1);
  gl_Position = vec4(pos.xyz, 1);

  vertexPos       = (instanceMatrix * vec3(aPos, 1)).xy;
  vertexColor     = instanceFill;
  vertexFrom      = instanceFrom;
  vertexTo        = instanceTo;
  vertexThickness = instanceThickness;
}
