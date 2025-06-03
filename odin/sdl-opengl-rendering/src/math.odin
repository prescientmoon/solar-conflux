package visuals

ℝ :: f32
ℝ² :: [2]ℝ
ℝ³ :: [3]ℝ
ℕ :: uint
Mat3 :: matrix[3, 3]ℝ
Color :: [4]ℝ

// Get a vector perpendicular to the given input
vec2_perp :: proc(v: ℝ²) -> ℝ² {
	return {-v.y, v.x}
}
