import { Option } from '../types'
import { toArray } from './toArray'

/**
 * Take all the values that are present, throwing away any None
 *
 * @param iterable The iterable to collect the values from.
 */
export const values = <T>(iterable: Iterable<Option<T>>) => {
    return Array.from(iterable).flatMap(toArray)
}
