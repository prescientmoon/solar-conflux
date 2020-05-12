/**
 * Typesafe version of Object.values
 */
export const values = Object.values as <T extends keyof object, U>(
  object: Record<T, U>
) => U[];

/**
 * Library agnostic way of expressing a lens
 */
export type GenericLens<T, U> = {
  get: (v: T) => U;
  set: (v: T, n: U) => T;
};

/**
 * Some basic equality to not re-render with the same state
 */
export const areEqual = <T>(a: T, b: T) =>
  a === b || JSON.stringify(a) === JSON.stringify(b);
