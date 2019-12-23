import { some, none } from './internals'
import { Brand } from 'utility-types'

type None = Brand<void, typeof none>
type Some<T> = Brand<T, typeof some>

export type Option<T> = Some<T> | None

export const None = undefined as None
export const Some = <T>(value: T): Option<T> => value as Some<T>
