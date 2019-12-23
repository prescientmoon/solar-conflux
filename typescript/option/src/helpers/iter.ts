import { some } from '../internals'
import { Mapper } from '../internalTypes'
import { Option } from '../types'

export const iter = <T>(mapper: Mapper<T, void>, option: Option<T>) => {
    if (option.__brand === some) {
        mapper(option as T)
    }
}
