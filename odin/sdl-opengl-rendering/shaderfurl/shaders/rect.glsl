#include <shape>
#include <utils>

in vec2 i_center;
in vec2 i_dimensions;

void vert() {
  compute_shape_pos();
}

void frag() {
  FragColor = shape_color(sdf_rect(v_center, v_dimensions, v_pos));
}
