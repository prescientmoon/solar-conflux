struct Globals {
  time: f32,
  resolution: vec2<f32>,
  padding: f32,
};

struct VSOut {
  @builtin(position) pos: vec4<f32>,
  @location(0) uv: vec2<f32>,
};

@group(0) @binding(0)
var<storage, read> globals: Globals;


@vertex
fn vs_main(
  @builtin(vertex_index) in_vertex_index: u32
) -> VSOut {
  let x = f32(i32(in_vertex_index) - 1);
  let y = f32(i32(in_vertex_index & 1u) * 2 - 1);
  let position = vec2(x, y);

  var out: VSOut;

  let scale = 0.5 + 0.5 * sin(globals.time);
  out.pos = vec4(position * scale, 0.0, 1.0);
  out.uv = position * globals.resolution;

  return out;
}

@fragment
fn fs_main(in: VSOut) -> @location(0) vec4<f32> {
  return vec4<f32>(1.0, 0.0, 0.0, 1.0);
}
