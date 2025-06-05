// This file contains types/constants/functions that are already implemented
// in my proper projects.
package visuals

ℝ :: f32
ℝ² :: [2]ℝ
ℝ³ :: [3]ℝ
ℕ :: uint
Mat3 :: matrix[3, 3]ℝ
Mat4 :: matrix[4, 4]ℝ
Color :: [4]ℝ
Affine2 :: matrix[2, 3]ℝ

AABB :: struct {
	top_left:   ℝ²,
	dimensions: ℝ²,
}

□ :: AABB

aabb_center :: proc(aabb: □) -> ℝ² {
	return aabb.top_left + aabb.dimensions / 2
}

Circle2 :: struct {
	center: ℝ²,
	radius: ℝ,
}

// Get a vector perpendicular to the given input
vec2_perp :: proc(v: ℝ²) -> ℝ² {
	return {-v.y, v.x}
}

@(private = "file")
g_state: State
g_renderer_state :: proc() -> ^State {
	return &g_state
}
