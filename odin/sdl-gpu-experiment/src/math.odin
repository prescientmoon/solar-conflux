package experiment

import "core:math/linalg"

Affine2 :: matrix[2, 3]f32
Mat2 :: matrix[2, 2]f32
Vec2 :: [2]f32

new_affine2 :: proc(mx: Mat2, translation: Vec2) -> (out: Affine2) {
	out[0] = mx[0]
	out[1] = mx[1]
	out[2] = translation

	return out
}

compose_affine2 :: proc(first, second: Affine2) -> (out: Affine2) {
	out[0] = first * ([3]f32){ second[0].x, second[0].y, 0 }
	out[1] = first * ([3]f32){ second[1].x, second[1].y, 0 }
	out[2] = first * ([3]f32){ second[2].x, second[2].y, 1 }

	return out
}

affine2_rotate_around :: proc(point: Vec2, angle: f32) -> Affine2 {
  return compose_affine2(
    new_affine2(linalg.matrix2_rotate(angle), point),
    new_affine2({ 1, 0, 0, 1}, -point),
  )
}
