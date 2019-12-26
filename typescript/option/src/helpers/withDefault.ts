import { unwrap } from './unwrap'
import { identity } from '@thi.ng/compose'
import { Option } from '../types'

/**
 * Provide a default value, turning an optional value into a normal value.
 *
 * @param _default The default value to use.
 * @param option The option to get the default of.
 */
export const withDefault = <T>(_default: T, option: Option<T>) => {
    return unwrap(_default, identity, option)
}
