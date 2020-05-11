/**
 * Typesafe version of Object.values
 */
export const values = Object.values as <T extends keyof object, U>(
  object: Record<T, U>
) => U[];
