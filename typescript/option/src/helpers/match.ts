import { Option } from '../types'
import { Mapper } from '../internalTypes'
import { isSome } from './isSome'

export const match = <T, U>(
    caseSome: Mapper<T, U>,
    _default: U,
    option: Option<T>
) => {
    if (isSome(option)) {
        return caseSome(option as T)
    }

    return _default
}
