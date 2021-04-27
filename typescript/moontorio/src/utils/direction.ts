import { Direction } from "./types";

export const next = (direction: Direction): Direction => (direction + 1) % 4;
export const prev = (direction: Direction): Direction => (direction + 3) % 4;
