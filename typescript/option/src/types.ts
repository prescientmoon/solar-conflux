import { none, identity } from './internals'
import { Brand } from 'utility-types'

// This is never actually used outside of typing so we can just declare it
declare const some: unique symbol

type None = Brand<void, typeof none>
type Some<T> = Brand<T, typeof some>

export type Option<T> = Some<T> | None

export const None = {
    __brand: none,
    toString: () => 'None'
} as None
export const Some = identity as <T>(value: T) => Option<T>
