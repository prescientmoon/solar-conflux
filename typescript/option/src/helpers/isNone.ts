import { Option } from '../types'
import { none } from '../internals'

export const isNone = <T>(option: Option<T>) => option.__brand === none
