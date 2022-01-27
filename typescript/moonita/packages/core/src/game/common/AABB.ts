import * as V from "./Vector";

export interface AABB {
  position: V.Vector2;
  size: V.Vector2;
}

/** Convert a bouinding box to a rectangle structure */
export function toRect(aabb: AABB) {
  return {
    x: aabb.position.x,
    y: aabb.position.y,
    width: aabb.size.x,
    height: aabb.size.y,
  };
}

/** Same as pointInside but does not take a vector */
export function rawPointInside(aabb: AABB, x: number, y: number) {
  return (
    x >= aabb.position.x &&
    x <= aabb.position.x + aabb.size.x &&
    y >= aabb.position.y &&
    y <= aabb.position.y + aabb.size.y
  );
}

/** Checks whether a point is inside a bounding box */
export function pointInside(aabb: AABB, point: V.Vector2) {
  return rawPointInside(aabb, point.x, point.y);
}

/**
 * Create a square shaped bounding box based on the
 * the center of the square and half it's side length
 */
export function fromSquareCenter(center: V.Vector2, radius: number): AABB {
  return {
    position: {
      x: center.x - radius,
      y: center.y - radius,
    },
    size: {
      x: radius * 2,
      y: radius * 2,
    },
  };
}

/** Calculate the center point of the bounding box */
export function center(aabb: AABB): V.Vector2 {
  return {
    x: aabb.position.x + aabb.size.x / 2,
    y: aabb.position.y + aabb.size.y / 2,
  };
}
