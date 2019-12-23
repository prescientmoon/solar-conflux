import { bind } from './bind'
import { identity } from '@thi.ng/compose'
import { Option } from '../types'

export const flat = <T>(option: Option<Option<T>>): Option<T> => {
    return bind(identity, option)
}
