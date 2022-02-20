#version 300 es
precision highp float;

in vec2 v_coord;

uniform sampler2D tex;

out vec4 outColor;

void main() {
    outColor = texture(tex, v_coord);
}