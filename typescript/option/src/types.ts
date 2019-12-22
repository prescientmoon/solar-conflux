import { NominalTyped, none, some } from './internals'

export type None = NominalTyped<typeof none, null>
export type Some<T> = NominalTyped<typeof some, T>

export type Option<T> = Some<T> | None

export const None: Option<any> = {
    type: none,
    value: null
}

export const Some = <T>(value: T): Option<T> => ({
    type: some,
    value
})
