import { Mapper } from '../internalTypes'
import { Option } from '../types'
import { withDefault } from './withDefault'
import { map } from './map'

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
    mapper: Mapper<T, U>,
    option: Option<T>
) => {
    return withDefault(_default, map(mapper, option))
}
