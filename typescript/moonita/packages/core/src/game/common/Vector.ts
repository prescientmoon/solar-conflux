// ========== Types
export interface Vector2 {
  x: number;
  y: number;
}

// ========== Helpers

export const vectorDifference = (a: Vector2, b: Vector2): Vector2 => ({
  x: a.x - b.x,
  y: a.y - b.y,
});

export const vectorLength = (a: Vector2) => Math.sqrt(a.x ** 2 + a.y ** 2);

/** Multiply each component of a vector by the coresponding component in another vector */
export const scaleVectorMut = (
  into: Vector2,
  vec: Vector2,
  by: number
): Vector2 => {
  into.x = vec.x * by;
  into.y = vec.y * by;

  return into;
};

/** Multiply each component of a vector by the coresponding component in another vector */
export const scaleVector = (vec: Vector2, by: number): Vector2 => ({
  x: vec.x * by,
  y: vec.y * by,
});

export const scaleVectorPerAxis = (a: Vector2, by: Vector2): Vector2 => ({
  x: a.x * by.x,
  y: a.y * by.y,
});

/** Run a function over each element of a vector */
export const mapVector = (
  a: Vector2,
  mapper: (n: number) => number
): Vector2 => ({
  x: mapper(a.x),
  y: mapper(a.y),
});

export const addVectorsMut = (into: Vector2, a: Vector2, b: Vector2) => {
  into.x = a.x + b.x;
  into.y = a.y + b.y;

  return into;
};

export const subVectorsMut = (into: Vector2, a: Vector2, b: Vector2) => {
  into.x = a.x - b.x;
  into.y = a.y - b.y;

  return into;
};

/** Flip a vector on both axis */
export const flip = (vec: Vector2): Vector2 => ({
  x: vec.x * -1,
  y: vec.y * -1,
});
