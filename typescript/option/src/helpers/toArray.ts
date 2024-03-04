import { unwrap } from './unwrap'
import { Option } from '../types'

export const toArray = <T>(option: Option<T>) => {
    return unwrap([], v => [v], option)
}
