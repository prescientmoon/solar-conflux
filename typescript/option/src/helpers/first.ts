import { isSome } from './isSome'
import { Option, None } from '../types'

/**
 * Returns the first Some in an iterable. If there isn't any returns None.
 *
 * @param elemenets The elements to find the first Some in.
 */
export const first = <T>(elemenets: Iterable<Option<T>>) => {
    for (const option of elemenets) {
        if (isSome(option)) {
            return option
        }
    }

    return None
}
