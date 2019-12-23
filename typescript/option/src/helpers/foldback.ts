import { match } from './match'
import { Option } from '../types'
import { BackFolder } from '../internalTypes'

export const foldback = <T, U>(
    folder: BackFolder<T, U>,
    option: Option<T>,
    initial: U
) => {
    return match(v => folder(v, initial), initial, option)
}
