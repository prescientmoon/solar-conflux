layout(std140, binding = 0) uniform Globals {
  mat4 u_viewport_matrix;
  float u_aa_width; // Anti aliasing width
};
