import { some, none } from './internals'

type NominalTyped<T, U> = {
    _type: T
    value: U
}

export type None = NominalTyped<typeof none, null>
export type Some<T> = NominalTyped<typeof some, T>

export type Option<T> = Some<T> | None

export const None: Option<any> = {
    _type: none,
    value: null
}

export const Some = <T>(value: T): Option<T> => ({
    _type: some,
    value
})
