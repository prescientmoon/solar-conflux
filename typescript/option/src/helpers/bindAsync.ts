import { Mapper } from '../internalTypes'
import { Option, None } from '../types'
import { unwrap } from './unwrap'

export const bindAsync = <T, U>(
    binder: Mapper<T, Promise<Option<U>>>,
    option: Option<T>
): Promise<Option<U>> => {
    return unwrap(Promise.resolve(None), binder, option)
}
