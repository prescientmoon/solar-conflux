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
