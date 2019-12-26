import { unwrap } from './unwrap'
import { Option } from '../types'
import { BackFolder } from '../internalTypes'

export const foldback = <T, U>(
    folder: BackFolder<T, U>,
    option: Option<T>,
    initial: U
) => {
    return unwrap(initial, v => folder(v, initial), option)
}
