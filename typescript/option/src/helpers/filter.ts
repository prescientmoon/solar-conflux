import { match } from './match'
import { Some, None, Option } from '../types'
import { Predicate } from '../internalTypes'

export const filter = <T>(predicate: Predicate<T>, option: Option<T>) => {
    return match(v => (predicate(v) ? Some(v) : None), None, option)
}
