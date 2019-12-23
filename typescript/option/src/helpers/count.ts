import { isSome } from './isSome'
import { compL } from '@thi.ng/compose'

export const count = compL(isSome, Number)
