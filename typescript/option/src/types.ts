import { some, none } from './internals'
import { Brand } from 'utility-types'

type None = Brand<void, typeof none>
type Some<T> = Brand<T, typeof some>

export type Option<T> = Some<T> | None

export const None = { __brand: none } as None
export const Some = <T>(value: T) => value as Option<T>
