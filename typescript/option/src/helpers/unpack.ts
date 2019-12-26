import { Lazy, Mapper } from '../internalTypes'
import { Option } from '../types'
import { withDefaultLazy } from './withDefaultLazy'
import { map } from './map'

/**
 * Like unwrap, but the default value is lazy,
 * and will only be computed if the Option is None.
 *
 * @param _default The lazy value to use in case option is None.
 * @param mapper The function to pass the inner value to.
 * @param option The option to unpack.
 */
export const unpack = <T, U>(
    _default: Lazy<U>,
    mapper: Mapper<T, U>,
    option: Option<T>
) => {
    return withDefaultLazy(_default, map(mapper, option))
}
