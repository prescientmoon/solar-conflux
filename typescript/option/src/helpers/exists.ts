import { unwrap } from './unwrap'
import { Predicate } from '../internalTypes'
import { Option } from '../types'

export const exists = <T>(predicate: Predicate<T>, option: Option<T>) => {
    return unwrap(false, predicate, option)
}
