#include "prelude/macros.slang"
import "prelude/main";

struct Sprite {
  float2x3 transform;
  // We can't use the rect types here because it leads to crashes :/
  float4 uvRect;
};

struct Globals {
  Sprite sprites[1024];
  uint _spriteCount; // Unused, here for the CPU's sake
  uint tick;
  float2 screenSize;
};

struct VOut {
  float4 pos : SV_Position;
  float2 uv;
};

SDL_TEXTURE(atlas, 0)
SDL_STORAGE(globals, Globals, 1)
#define globals globals_buf[0]

[shader("vertex")]
VOut vertMain(uint vix : SV_VertexID, uint iix : SV_InstanceID) {
  VOut output;

  Sprite sprite = globals.sprites[iix];
  float2 quadUV = quad.uv(vix);
  float2 spos = mul(sprite.transform, float3(quadUV, 1));
  output.pos = ndcFromScreen(spos, globals.screenSize);
  output.uv = Rect(sprite.uvRect).mapUV(quadUV);

  return output;
}

[shader("fragment")]
float4 fragMain(VOut input) : SV_Target {
  return atlas.Sample(input.uv);
}
