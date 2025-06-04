#version 330

out vec4 FragColor;

in vec4 vertexColor;
in vec2 vertexPos;
in vec2 vertexFrom;
in vec2 vertexTo;
in float vertexThickness;

layout(std140) uniform Globals {
  mat4 viewportMatrix;
  float aaWidth;
};

float sdfRoundedLine(vec2 p) {
  vec2 a = vertexFrom;
  vec2 b = vertexTo;
  vec2 pa = p-a, ba = b-a;

  float h = clamp(dot(pa,ba)/dot(ba, ba), 0.0, 1.0);
  return length(pa - ba * h) - vertexThickness;
}

void main() {
  float dist = sdfRoundedLine(vertexPos);
  float alpha = smoothstep(aaWidth, -aaWidth, dist);

  if (alpha < 0.001) discard;
  FragColor = vec4(vertexColor.xyz, alpha * vertexColor.a);
}
