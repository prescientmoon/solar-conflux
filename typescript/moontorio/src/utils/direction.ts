import { Direction, Vec2 } from "./types";

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

export const addDirection = (position: Vec2, direction: Direction): Vec2 => {
  if (direction === Direction.Down) return [position[0], position[1] + 1];
  else if (direction === Direction.Up) return [position[0], position[1] - 1];
  else if (direction === Direction.Right) return [position[0] + 1, position[1]];
  else if (direction === Direction.Left) return [position[0] - 1, position[1]];

  throw new Error(`Invalid direction ${direction}`);
};

// Attempts to find a direction leading from origin -> point
// Assumes the points are different
export const relativeTo = (origin: Vec2, point: Vec2): Direction | null => {
  if (origin[0] === point[0])
    return origin[1] > point[1] ? Direction.Up : Direction.Down;
  if (origin[1] === point[1])
    return origin[0] > point[0] ? Direction.Left : Direction.Right;

  return null;
};
