// ========== Types
export interface Vector2 {
  x: number;
  y: number;
}

// ========== Helpers

export const difference = (a: Vector2, b: Vector2): Vector2 => ({
  x: a.x - b.x,
  y: a.y - b.y,
});

export const length = (a: Vector2) => Math.sqrt(a.x ** 2 + a.y ** 2);

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
