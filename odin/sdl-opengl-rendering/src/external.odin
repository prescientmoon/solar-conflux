// This file contains types/constants/functions that are already implemented
// in my proper projects, and have been copy-pasted here on a per-need basis.
package visuals

import "base:intrinsics"
import "core:math"

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

ε: ℝ : 0.0001
π: ℝ : math.π
∞ := math.inf_f32(1)

// Checks whether two values are "close enough to be considered equal".
close_enough :: proc(a, b: $T) -> bool where intrinsics.type_is_numeric(T) {
	when intrinsics.type_is_array(T) {
		return linalg.length2(a - b) < ε * ε
	} else {
		return abs(a - b) < ε
	}
}

≃ :: close_enough

@(private = "file")
g_state: State
g_renderer_state :: proc() -> ^State {
	return &g_state
}
