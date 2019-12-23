import { Option } from '../types'
import { Mapper } from '../internalTypes'
import { some } from '../internals'

export const match = <T, U>(
    caseSome: Mapper<T, U>,
    _default: U,
    option: Option<T>
) => {
    if (option.__brand === some) {
        return caseSome(option as T)
    }

    return _default
}
