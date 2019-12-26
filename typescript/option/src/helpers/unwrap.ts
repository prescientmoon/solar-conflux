import { Option } from '../types'
import { Mapper } from '../internalTypes'
import { isSome } from './isSome'

/**
 * Apply the function to the value in the Maybe and return it unwrapped.
 * If the Maybe is Nothing, use the default value instead.
 *
 * @param _default The default value to use.
 * @param mapper Function to apply to the inner value.
 * @param option Option to unwrap.
 */
export const unwrap = <T, U>(
    _default: U,
    caseSome: Mapper<T, U>,
    option: Option<T>
) => {
    if (isSome(option)) {
        return caseSome(option as T)
    }

    return _default
}
