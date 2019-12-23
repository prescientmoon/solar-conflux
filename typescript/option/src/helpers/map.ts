import { match } from './match'
import { Mapper } from '../internalTypes'
import { Option, Some, None } from '../types'
import { compL } from '@thi.ng/compose'

export const map = <T, U>(
    mapper: Mapper<T, U>,
    option: Option<T>
): Option<U> => {
    return match(compL(mapper, Some), None, option)
}
