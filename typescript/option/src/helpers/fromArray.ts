import { None, Some, Option } from '../types'

export const fromArray = <T>(value: [T] | []): Option<T> => {
    return value[0] === undefined ? None : Some(value[0])
}
