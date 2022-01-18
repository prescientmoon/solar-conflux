import { Vector2 } from "./Vector";
import * as V from "./Vector";

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

export const toLocalCoordinates = (transform: Transform, vec: Vector2) => {
  const result = V.clone(vec);

  result.x /= transform.scale.x;
  result.y /= transform.scale.y;

  result.x -= transform.position.x;
  result.x -= transform.position.y;

  return V.rotate(result, transform.rotation);
};

export const toGlobalCoordinates = (transform: Transform, vec: Vector2) => {
  const result = V.rotate(vec, -transform.rotation);

  result.x += transform.position.x;
  result.y += transform.position.y;

  result.x *= transform.scale.x;
  result.y *= transform.scale.y;

  return result;
};
