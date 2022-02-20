#version 300 es
precision highp float;

uniform vec4 u_color;

in vec2 v_coord;
out vec4 outColor;

void main() {
    bool condition = length(v_coord) < 1.0;

    outColor = vec4(u_color.xyz, float(condition) * u_color.w);
}