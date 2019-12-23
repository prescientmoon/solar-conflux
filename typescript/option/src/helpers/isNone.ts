import { Option } from '../types'
import { none } from '../internals'

export const isNothing = <T>(option: Option<T>) => option.__brand === none
