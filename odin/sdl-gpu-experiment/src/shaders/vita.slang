#include "prelude/macros.slang"
import "prelude/main";

struct VOut {
  float4 pos : SV_Position;
  float2 uv;
};

SDL_TEXTURE(vita, 0)

[shader("vertex")]
VOut vertMain(uint vix : SV_VertexID, uint iix : SV_InstanceID) {
  VOut output;

  float2 quadUV = quad.uv(vix);
  output.pos = ndcFromScreen(quadUV, 1);
  output.uv = quadUV;

  return output;
}

[shader("fragment")]
float4 fragMain(VOut input) : SV_Target {
  return vita.Sample(input.uv);
}
