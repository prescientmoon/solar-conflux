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
