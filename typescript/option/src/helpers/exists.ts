import { match } from './match'
import { Predicate } from '../internalTypes'
import { Option } from '../types'

export const exists = <T>(predicate: Predicate<T>, option: Option<T>) => {
    return match(predicate, false, option)
}
