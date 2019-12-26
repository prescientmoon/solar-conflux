import { Binder } from '../internalTypes'
import { Option, None } from '../types'
import { unwrap } from './unwrap'

export const bind = <T, U>(
    binder: Binder<T, U>,
    option: Option<T>
): Option<U> => {
    return unwrap(None, binder, option)
}
