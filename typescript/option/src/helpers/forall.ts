import { unwrap } from './unwrap'
import { Predicate } from '../internalTypes'
import { Option } from '../types'

export const forall = <T>(predicate: Predicate<T>, option: Option<T>) => {
    return unwrap(true, predicate, option)
}
