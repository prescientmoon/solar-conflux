import { Option } from '../types'
import { isSome } from './isSome'

/**
 * Returns the first value that is present, like the boolean ||.
 * Both values will be computed.
 * There is no short-circuiting.
 * If your second argument is expensive to calculate and
 * you need short circuiting, use orLazy instead.
 *
 * @param a The first argument.
 * @param b The second argument.
 */
export const or = <T>(a: Option<T>, b: Option<T>): Option<T> => {
    if (isSome(a)) {
        return a
    } else {
        return b
    }
}
