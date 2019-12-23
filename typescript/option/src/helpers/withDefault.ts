import { match } from './match'
import { identity } from '../internals'
import { Option } from '../types'

export const withDefault = <T>(_default: T, option: Option<T>) => {
    return match(identity, _default, option)
}
