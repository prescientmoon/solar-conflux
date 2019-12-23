import { Mapper } from '../internalTypes'
import { Option } from '../types'
import { isSome } from './isSome'

export const iter = <T>(mapper: Mapper<T, void>, option: Option<T>) => {
    if (isSome(option)) {
        mapper(option as T)
    }
}
