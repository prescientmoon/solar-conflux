import { NominalTyped, none, some } from './internals'

export type None = NominalTyped<'none', null>
export type Some<T> = NominalTyped<'some', T>

export type Option<T> = Some<T> | None

export const None: Option<any> = {
    _type: 'none',
    value: null
}

export const Some = <T>(value: T): Option<T> => ({
    _type: 'some',
    value
})
