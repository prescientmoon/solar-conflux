// ========== types
/** number between 0 and 2 pi */
export type Angle = number;

// ========== Helpers
/**
 * Clamp a value between two other values.
 */
export const clamp = (x: number, min: number, max: number) => {
  return x < min ? min : x > max ? max : x;
};

/** Pi * 2 */
export const TAU = 2 * Math.PI;

/** Cast an angle to an angle between 0 and 2 * Pi */
export const normalizeAngle = (angle: number): number => {
  return angle < 0 ? TAU - normalizeAngle(-angle) : angle % TAU;
};

/**
 * Casts an angle to an angle between -Pi and Pi
 */
export const toDirectionalAngle = (angle: number) => {
  const normalized = normalizeAngle(angle);

  return normalized > Math.PI ? normalized - TAU : normalized;
};

/** Generate a random number inside a given range.
 * Does not check if the range is valid
 */
export function randomBetween(min: number, max: number) {
  return Math.random() * (max - min) + min;
}

/** Calculate the distance between 2 points, squared */
export function distanceSquared(
  x1: number,
  y1: number,
  x2: number,
  y2: number
): number {
  return (x1 - x2) ** 2 + (y1 - y2) ** 2;
}
