import { Mapper } from '../internalTypes'
import { Option, None } from '../types'
import { match } from './match'

export const bindAsync = <T, U>(
    binder: Mapper<T, Promise<Option<U>>>,
    option: Option<T>
): Promise<Option<U>> => {
    return match(binder, Promise.resolve(None), option)
}
