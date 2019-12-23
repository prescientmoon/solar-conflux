import { isSome } from './isSome'
import { Option } from '../types'

export const count = <T>(option: Option<T>) => Number(isSome(option))
