import { Option } from '../types'
import { isNothing } from './isNone'

export const isSome = <T>(option: Option<T>) => !isNothing(option)
