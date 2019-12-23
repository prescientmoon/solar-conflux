import { Option, None, Some } from '../types'
import { Mapper } from '../internalTypes'
import { match } from './match'

export const mapAsync = <T, U>(
    mapper: Mapper<T, Promise<U>>,
    option: Option<T>
) => {
    return match(
        async value => {
            const output = await mapper(value)

            return Some(output)
        },
        Promise.resolve(None),
        option
    )
}
