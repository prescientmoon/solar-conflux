implementing prelude;

public struct Rect {
  public float4 data;

  [ForceInline]
  public float2 mapUV(float2 uv) { return data.xy + uv * data.zw; }
};
