// Glorified boolean:)
export const enum AxisDirection {
  Negative,
  Positive,
}

/**
 * Returns the sign of the number. Returns positive if x == 0.
 */
export function getAxisDirection(x: number) {
  return x >= 0 ? AxisDirection.Positive : AxisDirection.Negative;
}
