module prelude;

__include rect;
__include quad;

[ForceInline]
public float4 ndcFromScreen(float2 pos, float2 screenSize) {
  return float4(pos / screenSize * 2 - 1, 0, 1);
}
