import { Option, None, Some } from '../types'
import { isNone } from './isNone'

/**
 * If every Option in the list is present, return all of the values unwrapped.
 * If there are any Nones, the whole function fails and returns None.
 *
 * @param iterable The iterable to combine.
 */
export const combine = <T>(iterable: Iterable<Option<T>>) => {
    const set = new Set(iterable)

    if (set.has(None)) {
        return None
    }

    const array = Array.from(set) as T[]
    return array as Option<T[]>
}
