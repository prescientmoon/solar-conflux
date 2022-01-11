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

export const vectorDifference = (a: Vector2, b: Vector2): Vector2 => ({
  x: a.x - b.x,
  y: a.y - b.y,
});

export const applyTransformToVector = (transform: Transform, vec: Vector2) => {
  const tx = vec.x + transform.position.x;
  const ty = vec.y + transform.position.y;

  const rx =
    tx * Math.cos(transform.rotation) - ty * Math.sin(transform.rotation);
  const ry =
    tx * Math.sin(transform.rotation) + ty * Math.cos(transform.rotation);

  return {
    x: rx * transform.scale.x,
    y: ry * transform.scale.y,
  };
};
