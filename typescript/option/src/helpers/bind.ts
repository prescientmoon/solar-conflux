import { Binder } from '../internalTypes'
import { Option, None } from '../types'
import { match } from './match'

export const bind = <T, U>(
    binder: Binder<T, U>,
    option: Option<T>
): Option<U> => {
    return match(binder, None, option)
}
