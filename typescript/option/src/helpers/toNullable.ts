import { unwrap } from './unwrap'
import { identity } from '@thi.ng/compose'
import { Option } from '../types'

export const toNullable = <T>(option: Option<T>) => {
    return unwrap(null, identity, option)
}
