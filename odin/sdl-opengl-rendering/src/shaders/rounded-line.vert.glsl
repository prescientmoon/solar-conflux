#version 430

layout (location = 0) in vec2  aPos;
layout (location = 1) in vec4  iFill;
layout (location = 2) in vec4  iStroke;
layout (location = 3) in float iStrokeThickness;
layout (location = 4) in mat3  iMatrix;
layout (location = 7) in vec2  iFrom;
layout (location = 8) in vec2  iTo;
layout (location = 9) in float iThickness;

out vec4  vFill;
out vec4  vStroke;
out float vStrokeThickness;
out vec2  vPos;
out vec2  vFrom;
out vec2  vTo;
out float vThickness;

layout(std140, binding = 0) uniform Globals {
  mat4 viewportMatrix;
  float aaWidth; // Anti aliasing width
};

void main() {
  vec4 pos    = viewportMatrix * vec4(iMatrix * vec3(aPos, 1), 1);
  gl_Position = vec4(pos.xyz, 1);

  vPos             = (iMatrix * vec3(aPos, 1)).xy;
  vFill            = iFill;
  vStroke          = iStroke;
  vStrokeThickness = iStrokeThickness;
  vFrom            = iFrom;
  vTo              = iTo;
  vThickness       = iThickness;
}
