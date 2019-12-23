import { match } from './match'
import { Predicate } from '../internalTypes'
import { Option } from '../types'

export const forall = <T>(predicate: Predicate<T>, option: Option<T>) => {
    return match(predicate, true, option)
}
