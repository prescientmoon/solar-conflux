import { randomBetween } from "../../math";

// ========== Types
export interface Vector2 {
  x: number;
  y: number;
}

export interface PolarVector2 {
  radius: number;
  angle: number;
}

// ========== Helpers

export const difference = (a: Vector2, b: Vector2): Vector2 => ({
  x: a.x - b.x,
  y: a.y - b.y,
});

export const lengthSquared = (a: Vector2) => a.x ** 2 + a.y ** 2;
export const length = (a: Vector2) => Math.sqrt(lengthSquared(a));

/** Multiply each component of a vector by the coresponding component in another vector */
export const scaleMut = (into: Vector2, vec: Vector2, by: number): Vector2 => {
  into.x = vec.x * by;
  into.y = vec.y * by;

  return into;
};

/** Multiply each component of a vector by a scalar */
export const scale = (vec: Vector2, by: number): Vector2 => ({
  x: vec.x * by,
  y: vec.y * by,
});

/** Multiply each component of a vector by the coresponding component in another vector */
export const scalePerAxis = (a: Vector2, by: Vector2): Vector2 => ({
  x: a.x * by.x,
  y: a.y * by.y,
});

/** Multiply each component of a vector by the coresponding component in another vector */
export function scalePerAxisMut(
  into: Vector2,
  a: Vector2,
  by: Vector2
): Vector2 {
  into.x = a.x * by.x;
  into.y = a.y * by.y;

  return into;
}

/** Run a function over each element of a vector */
export const map = (a: Vector2, mapper: (n: number) => number): Vector2 => ({
  x: mapper(a.x),
  y: mapper(a.y),
});

export const addMut = (into: Vector2, a: Vector2, b: Vector2) => {
  into.x = a.x + b.x;
  into.y = a.y + b.y;

  return into;
};

export function add(a: Vector2, b: Vector2): Vector2 {
  return {
    x: a.x + b.x,
    y: a.y + b.y,
  };
}

/** Substract two vectors */
export const sub = (a: Vector2, b: Vector2) => ({
  x: a.x - b.x,
  y: a.y - b.y,
});

/** Substract two vectors */
export const subMut = (into: Vector2, a: Vector2, b: Vector2) => {
  into.x = a.x - b.x;
  into.y = a.y - b.y;

  return into;
};

/** Flip a vector on both axis */
export const flip = (vec: Vector2): Vector2 => ({
  x: vec.x * -1,
  y: vec.y * -1,
});

/** Create a vector with the same coordinates as a given vector */
export const clone = (vec: Vector2): Vector2 => ({
  ...vec,
});

/** Rotate a vector around the origin by a given amount of radians */
export const rotate = (vec: Vector2, angle: number): Vector2 => {
  if (angle === 0) return vec;

  const x = vec.x * Math.cos(angle) - vec.y * Math.sin(angle);
  const y = vec.x * Math.sin(angle) + vec.y * Math.cos(angle);

  return { x, y };
};

/** Calculates the angle between 2 vectors
 *
 * Eg: angleBetween((1, 0), (0, 1)) == PI/2
 */
export function angleBetween(a: Vector2, b: Vector2): number {
  const alpha = Math.atan2(a.x, a.y);
  const beta = Math.atan2(b.x, b.y);

  return alpha - beta;
}

/** Calculate the distance between 2 points, squared */
export function distanceSquared(a: Vector2, b: Vector2): number {
  return (a.x - b.x) ** 2 + (a.y - b.y) ** 2;
}

/** Calculate the distnace between 2 points */
export function distance(a: Vector2, b: Vector2): number {
  return Math.sqrt(distanceSquared(a, b));
}

/** Make the length of a vector 1, while preserving the direction */
export function normalizeMut(into: Vector2, vec: Vector2): Vector2 {
  const l = length(vec);

  if (l === 0) throw new Error(`Cannot normalize origin vector!`);

  into.x = vec.x / l;
  into.y = vec.y / l;

  return into;
}

/**
 * Generate a random point inside the rectangle
 * described by two given points
 */
export function random2dBetween(
  bottomLeft: Vector2,
  topRight: Vector2
): Vector2 {
  return {
    x: randomBetween(bottomLeft.x, topRight.x),
    y: randomBetween(bottomLeft.y, topRight.y),
  };
}

/** Generate a random point inside a square with the center at the origin */
export function random2dInsideOriginSquare(neg: number, pos: number): Vector2 {
  return random2dBetween({ x: neg, y: neg }, { x: pos, y: pos });
}

export function limitMagnitudeMut(
  into: Vector2,
  vec: Vector2,
  limit: number
): Vector2 {
  const current = length(vec);

  if (current < limit) {
    into.x = vec.x;
    into.y = vec.y;
  } else {
    into.x = (vec.x / current) * limit;
    into.y = (vec.y / current) * limit;
  }

  return into;
}

/** The origin vector */
export function origin(): Vector2 {
  return { x: 0, y: 0 };
}

/** Calculate the dot product of 2 vectors */
export function dotProduct(a: Vector2, b: Vector2): number {
  return a.x * b.x + a.y * b.y;
}
