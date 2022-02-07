import { Vector2 } from "./Vector";

export interface PathPoint {
  position: Vector2;
}

export type Path = Array<PathPoint>;

export type PathId = number; // We want to be able to dinamically create paths (perhaps?)

// ========== Helpers
/** Reverse the order of the points in a path */
export function reversePath(path: Path): Path {
  const result = [...path];
  result.reverse();

  return result;
}
