import Quadtree from "@timohausmann/quadtree-js";
import * as V from "./Vector";

export interface AABB {
  position: V.Vector2;
  size: V.Vector2;
}

/** Convert a bouinding box to a rectangle structure */
export function toRect(aabb: AABB): Quadtree.Rect {
  return {
    x: aabb.position.x,
    y: aabb.position.y,
    width: aabb.size.x,
    height: aabb.size.y,
  };
}

/** Checks whether a point is inside a bounding box */
export function pointInside(aabb: AABB, point: V.Vector2) {
  return (
    point.x > aabb.position.x &&
    point.x < aabb.position.x + aabb.size.x &&
    point.y > aabb.position.y &&
    point.y < aabb.position.y + aabb.size.y
  );
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
