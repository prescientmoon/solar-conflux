import { match } from './match'
import { Option } from '../types'

export const toArray = <T>(option: Option<T>) => {
    return match(v => [v], [], option)
}
