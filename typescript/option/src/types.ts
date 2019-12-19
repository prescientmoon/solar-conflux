import * as Internals from './internals'

export type Option<T> = Internals.SomeClass<T> | Internals.NoneClass

export const None = new Internals.NoneClass()
export const Some = <T>(v: T) => new Internals.SomeClass(v)
