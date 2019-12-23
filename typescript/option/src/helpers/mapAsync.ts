import { Option, None, Some } from '../types'
import { Mapper } from '../internalTypes'
import { match } from './match'

export const mapAsync = <T, U>(
    mapper: Mapper<T, Promise<U>>,
    option: Option<T>
) => {
    return match(
        value => mapper(value).then(Some),
        Promise.resolve(None),
        option
    )
}
