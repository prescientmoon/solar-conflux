import { Option } from '../types'
import { isSome } from './isSome'

export const get = <T>(option: Option<T>): T => {
    if (isSome(option)) {
        return option as T
    }

    throw new Error(`Cannot get value of None`)
}
