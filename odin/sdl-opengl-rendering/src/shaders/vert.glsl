#version 430

layout (location = 0) in vec2  aPos;
layout (location = 1) in vec4  iFill;
layout (location = 2) in vec4  iStroke;
layout (location = 3) in float iStrokeThickness;
layout (location = 4) in mat3  iMatrix;

out vec4  vFill;
out vec4  vStroke;
out float vStrokeThickness;
out vec2  vPos;

layout(std140, binding = 0) uniform Globals {
  mat4 viewportMatrix;
  float aaWidth; // Anti aliasing width
};

void main() {
  vec4 pos    = viewportMatrix * vec4(iMatrix * vec3(aPos, 1), 1);
  gl_Position = vec4(pos.xyz, 1);

  vPos             = aPos.xy;
  vFill            = iFill;
  vStroke          = iStroke;
  vStrokeThickness = iStrokeThickness;
}
