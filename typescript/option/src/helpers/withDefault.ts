import { match } from './match'
import { identity } from '@thi.ng/compose'
import { Option } from '../types'

export const withDefault = <T>(_default: T, option: Option<T>) => {
    return match(identity, _default, option)
}
