import { isSome } from './isSome'
import { Option } from '../types'

/**
 * Lazy version of or.
 * The second argument will only be evaluated if the first argument is Nothing.
 *
 * @param a The first argument.
 * @param b The second argument.
 */
export const orLazy = <T>(a: Option<T>, b: () => Option<T>) => {
    if (isSome(a)) {
        return a
    }

    return b()
}
