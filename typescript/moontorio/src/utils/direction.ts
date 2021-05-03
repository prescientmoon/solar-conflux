import { Direction } from "./types";

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
