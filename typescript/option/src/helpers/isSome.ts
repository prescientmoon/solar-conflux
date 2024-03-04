import { Option } from '../types'
import { isNone } from './isNone'

export const isSome = <T>(option: Option<T>) => !isNone(option)
