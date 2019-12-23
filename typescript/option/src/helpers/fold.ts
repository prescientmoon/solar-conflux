import { match } from './match'
import { Option } from '../types'
import { Folder } from '../internalTypes'

export const fold = <T, U>(
    folder: Folder<T, U>,
    initial: U,
    option: Option<T>
) => {
    return match(v => folder(initial, v), initial, option)
}
