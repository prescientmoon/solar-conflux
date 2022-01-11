// ========== Types
export interface Vector2 {
  x: number;
  y: number;
}

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
