import { match } from './match'
import { identity } from '../internals'
import { Option } from '../types'

export const toNullable = <T>(option: Option<T>) => {
    return match(identity, null, option)
}
