import { Binder } from '../internalTypes'
import { isSome } from './isSome'
import { None } from '../types'

/**
 * Try a list of functions against a value.
 * Return the value of the first call that succeeds (aka returns Some).
 * If no function retursn Some this will default to None.
 *
 * @param input The input to pass to the functions.
 * @param functions Iterable of functions to try against the input.
 */
export const oneOf = <T, U>(input: T, functions: Binder<T, U>[]) => {
    for (const func of functions) {
        const result = func(input)

        if (isSome(result)) {
            return result
        }
    }

    return None
}
