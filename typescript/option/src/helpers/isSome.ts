import { Option } from '../types'
import { some } from '../internals'

export const isSome = <T>(option: Option<T>) => option.__brand === some
