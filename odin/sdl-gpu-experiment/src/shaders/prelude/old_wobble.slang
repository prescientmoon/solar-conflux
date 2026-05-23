// static const float3 COLORS[4] = {
//   float3(0.0, 0.0, 1.0),
//   float3(0.0, 1.0, 0.7),
//   float3(1.0, 1.0, 0.3),
//   float3(1.0, 0.0, 0.5),
// };
// float hash(float2 p) {
//   float2 q = frac(p * 0.6180339887) * 25.0;
//   return frac(q.x * q.y * (q.x + q.y));
// }
//
// float noise(float2 x) {
//   float2 p = floor(x);
//   float2 f = frac(x);
//
//   f = f * f * (3.0 - 2.0 * f);
//
//   float a = hash(p + float2(0, 0));
//   float b = hash(p + float2(1, 0));
//   float c = hash(p + float2(0, 1));
//   float d = hash(p + float2(1, 1));
//
//   return lerp(lerp(a, b, f.x), lerp(c, d, f.x), f.y);
// }
//
// float fbm(float2 x, float H) {
//   float G = exp2(-H);
//   float f = 1.0;
//   float a = 1.0;
//   float t = 0.0;
//
//   [unroll]
//   for (uint i = 0; i < 10; i++) {
//     t += a * noise(f * x);
//     f *= 2.0;
//     a *= G;
//   }
//
//   return t;
// }
// float time = float(globals.tick) / 60;
//
// float2 pv = (input.uv) * (1 + time / 10);
// float fancyv = fbm(pv + fbm(pv + fbm(pv, 1 + (0.2 * sin(time * 1.7 + 200))), 0.5 + (0.5 * sin(time))), 0.8 + (0.1 * sin(time * 3.3 + 20)));
// float4 sampled = atlas.Sample(input.uv + fancyv / 10);
//
// float4 icolor = float4(input.color, 1);
// float2 p = (icolor.xy - 0.5) * (1 + time / 10);
// float color = fbm(p + fbm(p + fbm(p, 1 + (0.2 * sin(time * 1.7 + 200))), 0.5 + (0.5 * sin(time))), 0.8 + (0.1 * sin(time * 3.3 + 20)));
//
// float4 fancy = float4(color, 0, icolor.z, 1);
//
// return saturate(sampled);
