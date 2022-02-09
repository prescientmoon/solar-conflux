import * as V from "./Vector";

export interface PathPoint {
  position: V.Vector2;
}

export type Path = {
  points: Array<PathPoint>;
  radius: number;
};

export type PathId = number; // We want to be able to dinamically create paths (perhaps?)

// ========== Helpers
export function mapPointArray(
  path: Path,
  mapper: (points: Path["points"]) => Path["points"]
): Path {
  return {
    points: mapper(path.points),
    radius: path.radius,
  };
}

export function mapPoints(
  path: Path,
  mapper: (points: PathPoint) => PathPoint
): Path {
  return {
    points: path.points.map(mapper),
    radius: path.radius,
  };
}
/** Reverse the order of the points in a path */
export function reversePath(path: Path): Path {
  return mapPointArray(path, (points) => [...points].reverse());
}

/** Flip a path relatie to the x=y line */
export function flip(path: Path): Path {
  return mapPoints(path, (p) => ({
    position: V.flip(p.position),
  }));
}
