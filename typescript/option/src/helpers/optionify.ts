import { fromNullable } from './fromNullable'

/**
 * Takes a function which returns a nullable and creates
 * a function which returns an Option.
 * In functional programming this would be the same as
 * composing the function with fromNullable.
 *
 * @param f The function to optionify
 */
export const optionify = <T extends unknown[], U>(
    f: (...args: T) => U | null
) => {
    return (...args: T) => fromNullable(f(...args))
}
