implementing prelude;

namespace quad {
  public static const float2 VERTICES[4] = {
    float2(0, 0), float2(0, 1), float2(1, 1), float2(1, 0),
  };

  public static const uint INDICES[6] = { 0, 1, 2, 0, 3, 2 };

  [ForceInline]
  public float2 uv(uint ix) { return VERTICES[INDICES[ix]]; }
};
