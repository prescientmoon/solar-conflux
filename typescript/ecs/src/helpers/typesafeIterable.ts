export const typesafeIterable = <T>(array: unknown[]) => array as T[]

export const typesafeKeys = <T extends object>(object: T) =>
  typesafeIterable<keyof T>(Object.keys(object))

export const typesafeEntries = <T extends object>(object: T) =>
  typesafeIterable<[keyof T, T[keyof T]]>(Object.entries(object))
