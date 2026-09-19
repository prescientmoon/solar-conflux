import { scale, Vector2 } from "./Vector";

// ========== Types
/** A camera is a transform with no rotation */
export interface Camera {
  position: Vector2;
  scale: Vector2;
}

// ========== Constants
export const identityCamera = (): Camera => {
  return {
    position: { x: 0, y: 0 },
    scale: { x: 1, y: 1 },
  };
};

/** Transform which fixes the y-flipped default canvas coordinate system */
export function defaultScreenTransform(): Camera {
  return flipYMut(identityCamera());
}

// ========== Helpers
export function translateLocalCoordinatesMut(camera: Camera, delta: Vector2) {
  camera.position.x += delta.x * camera.scale.x;
  camera.position.y += delta.y * camera.scale.y;

  return camera;
}

/** Switch a position to local scaling and then set the origin there */
export function setPositionGlobalCoordinatesMut(
  camera: Camera,
  position: Vector2
) {
  camera.position.x = position.x;
  camera.position.y = position.y;

  return camera;
}

export function translateGlobalCoordinatesMut(camera: Camera, delta: Vector2) {
  camera.position.x += delta.x;
  camera.position.y += delta.y;

  return camera;
}

export function scaleMut(camera: Camera, by: Vector2) {
  camera.scale.x *= by.x;
  camera.scale.y *= by.y;
  return camera;
}

export function scaleAroundLocalPointMut(
  camera: Camera,
  at: Vector2,
  by: Vector2
) {
  translateLocalCoordinatesMut(camera, at);
  scaleMut(camera, by);
  translateLocalCoordinatesMut(camera, scale(at, -1));

  return camera;
}

export function scaleAroundGlobalPointMut(
  camera: Camera,
  at: Vector2,
  by: Vector2
) {
  return scaleAroundLocalPointMut(camera, toLocalCoordinates(camera, at), by);
}

export function toLocalCoordinates(camera: Camera, point: Vector2): Vector2 {
  return {
    x: (point.x - camera.position.x) / camera.scale.x,
    y: (point.y - camera.position.y) / camera.scale.y,
  };
}

export function toGlobalCoordinates(camera: Camera, point: Vector2): Vector2 {
  return {
    x: point.x * camera.scale.x + camera.position.x,
    y: point.y * camera.scale.y + camera.position.y,
  };
}

/** Flip a camera on the x axis */
export function flipXMut(camera: Camera) {
  camera.scale.x *= -1;
  return camera;
}

/** Flip a camera on the y axis */
export function flipYMut(camera: Camera) {
  camera.scale.y *= -1;
  return camera;
}

/** Scaling a vector AB is conceptually equivalent to toLocalCoordinates(B) - toLocalCoordinates(a) */
export function toLocalScaleMut(camera: Camera, point: Vector2): Vector2 {
  point.x /= camera.scale.x;
  point.y /= camera.scale.y;

  return point;
}
