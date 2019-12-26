import { unwrap } from './unwrap'
import { Option } from '../types'
import { Folder } from '../internalTypes'

export const fold = <T, U>(
    folder: Folder<T, U>,
    initial: U,
    option: Option<T>
) => {
    return unwrap(initial, v => folder(initial, v), option)
}
