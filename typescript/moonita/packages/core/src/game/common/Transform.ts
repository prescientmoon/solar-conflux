import { Vector2 } from "./Vector";

// ========== Types
export interface Transform {
  rotation: number;
  position: Vector2;
  scale: Vector2;
}

// ========== Constants
export const identityTransform = (): Transform => ({
  rotation: 0,
  position: { x: 0, y: 0 },
  scale: { x: 1, y: 1 },
});

// ========== Helpers
export const flipXMut = (transform: Transform) => {
  transform.scale.x *= -1;
  return transform;
};

export const flipYMut = (transform: Transform) => {
  transform.scale.y *= -1;
  return transform;
};

export const applyTransformToVector = (transform: Transform, vec: Vector2) => {
  const tx = vec.x + transform.position.x;
  const ty = vec.y + transform.position.y;

  let rx = tx;
  let ry = ty;

  // Only compute trig functions when necessary
  if (transform.rotation !== 0) {
    rx = tx * Math.cos(transform.rotation) - ty * Math.sin(transform.rotation);
    ry = tx * Math.sin(transform.rotation) + ty * Math.cos(transform.rotation);
  }

  return {
    x: rx * transform.scale.x,
    y: ry * transform.scale.y,
  };
};

export const unApplyTransformToVector = (
  transform: Transform,
  vec: Vector2
) => {
  const sx = vec.x / transform.scale.x;
  const sy = vec.x / transform.scale.x;

  let rx = sx;
  let ry = sy;

  // Only compute trig functions when necessary
  if (transform.rotation !== 0) {
    rx =
      sx * Math.cos(-transform.rotation) - sy * Math.sin(-transform.rotation);
    ry =
      sx * Math.sin(-transform.rotation) + sy * Math.cos(-transform.rotation);
  }

  return {
    x: rx - transform.position.x,
    y: ry - transform.position.y,
  };
};
