import { unwrap } from './unwrap'
import { Some, None, Option } from '../types'
import { Predicate } from '../internalTypes'

export const filter = <T>(predicate: Predicate<T>, option: Option<T>) => {
    return unwrap(None, v => (predicate(v) ? Some(v) : None), option)
}
