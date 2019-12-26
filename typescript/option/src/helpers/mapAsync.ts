import { Option, None, Some } from '../types'
import { Mapper } from '../internalTypes'
import { unwrap } from './unwrap'

export const mapAsync = <T, U>(
    mapper: Mapper<T, Promise<U>>,
    option: Option<T>
) => {
    return unwrap(
        Promise.resolve(None),
        value => mapper(value).then(Some),
        option
    )
}
