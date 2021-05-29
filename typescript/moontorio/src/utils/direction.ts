import {
  MATH,
  mulN2,
  rotateAroundPoint2,
  rotateS2,
  Vec2Like,
} from "@thi.ng/vectors";

export const enum Direction {
  Right,
  Down,
  Left,
  Up,
}

export const next = (direction: Direction): Direction => (direction + 1) % 4;
export const prev = (direction: Direction): Direction => (direction + 3) % 4;
export const opposite = (direction: Direction): Direction =>
  (direction + 2) % 4;
export const onXAxis = (direction: Direction) => direction % 2 == 0;
export const onYAxis = (direction: Direction) => direction % 2 == 1;

export const directions = [
  Direction.Right,
  Direction.Down,
  Direction.Left,
  Direction.Up,
];

export const addDirection = (
  position: Vec2Like,
  direction: Direction
): Vec2Like => {
  if (direction === Direction.Down) return [position[0], position[1] + 1];
  else if (direction === Direction.Up) return [position[0], position[1] - 1];
  else if (direction === Direction.Right) return [position[0] + 1, position[1]];
  else if (direction === Direction.Left) return [position[0] - 1, position[1]];

  throw new Error(`Invalid direction ${direction}`);
};

/**
 * Rotates a direction reltive to another.
 * Assuming each direction adds pi/2 on the previous one:
 * - Right = 0
 * - Down = pi/2
 * - Left = pi
 * - Up = 3*pi/2
 */
export const relativeTo = (first: Direction, second: Direction): Direction =>
  (first + second) % 4;

// Attempts to find a direction leading from origin -> point
// Assumes the points are different
export const fromPositions = (
  origin: Vec2Like,
  point: Vec2Like
): Direction | null => {
  if (origin[0] === point[0])
    return origin[1] > point[1] ? Direction.Up : Direction.Down;
  if (origin[1] === point[1])
    return origin[0] > point[0] ? Direction.Left : Direction.Right;

  return null;
};

export const directionToAngle = (direction: Direction) =>
  (direction * Math.PI) / 2;

export const directionToUnitVector = (direction: Direction): Vec2Like =>
  addDirection([0, 0], direction);

export const directionToVector = (
  direction: Direction,
  magnitude: number
): Vec2Like => mulN2(null, directionToUnitVector(direction), magnitude);
