import { unwrap } from './unwrap'
import { Mapper } from '../internalTypes'
import { Option, Some, None } from '../types'

export const map = <T, U>(
    mapper: Mapper<T, U>,
    option: Option<T>
): Option<U> => {
    return unwrap(None, v => Some(mapper(v)), option)
}
