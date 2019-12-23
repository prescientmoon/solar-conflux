import { match } from './match'
import { Mapper } from '../internalTypes'
import { Option, Some, None } from '../types'

export const map = <T, U>(
    mapper: Mapper<T, U>,
    option: Option<T>
): Option<U> => {
    return match(v => Some(mapper(v)), None, option)
}
