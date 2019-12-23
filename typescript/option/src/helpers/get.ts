import { some } from '../internals'
import { Option } from '../types'

export const get = <T>(option: Option<T>): T => {
    if (option.__brand === some) {
        return option as T
    }

    throw new Error(`Cannot get value of None`)
}
