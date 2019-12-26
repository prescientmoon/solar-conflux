import { Option } from '../types'
import { isSome } from './isSome'
import { Lazy } from '../internalTypes'
import { get } from './get'

/**
 * Same as withDefault but the default is only evaluated when the option is None.
 *
 * @param _default Function returning the default value to use.
 * @param option The option to get the default of.
 */
export const withDefaultLazy = <T>(_default: Lazy<T>, option: Option<T>) => {
    if (isSome(option)) {
        return get(option)
    } else return _default()
}
