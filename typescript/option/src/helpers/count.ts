import { Option } from '../types'
import { isSome } from './isSome'

export const count = <T>(option: Option<T>) => Number(isSome(option))
