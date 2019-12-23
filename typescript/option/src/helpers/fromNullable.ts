import { Nullable } from '../internalTypes'
import { Some, None, Option } from '../types'

export const fromNullable = <T>(value: Nullable<T>): Option<T> => {
    return value === null ? None : Some(value)
}
