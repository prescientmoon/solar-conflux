import { Option, Some, None } from './types'
import { Binder, Folder, Mapper, Predicate, BackFolder } from './internalTypes'
import Internals, { SomeClass } from './internals'

export const isSome = <T>(option: Option<T>) =>
    option instanceof Internals.SomeClass
export const isNothing = <T>(option: Option<T>) =>
    option instanceof Internals.NoneClass

export const match = <T, U>(
    option: Option<T>,
    caseSome: (v: T) => U,
    caseNone: () => U
) => {
    if (isSome(option)) {
        return caseSome((option as SomeClass<T>)[Internals.someValue])
    }

    return caseNone()
}

export const bind = <T, U>(
    binder: Binder<T, U>,
    option: Option<T>
): Option<U> => {
    return match(option, binder, () => None)
}

export const map = <T, U>(
    mapper: Mapper<T, U>,
    option: Option<T>
): Option<U> => {
    return match(
        option,
        v => Some(mapper(v)),
        () => None
    )
}

export const count = <T>(option: Option<T>) => Number(isSome(option))

export const exists = <T>(predicate: Predicate<T>, option: Option<T>) => {
    return match(option, predicate, () => false)
}

export const filter = <T>(predicate: Predicate<T>, option: Option<T>) => {
    return match(
        option,
        v => (predicate(v) ? Some(v) : None),
        () => None
    )
}

export const fold = <T, U>(
    folder: Folder<T, U>,
    initial: U,
    option: Option<T>
) => {
    match(
        option,
        v => folder(initial, v),
        () => initial
    )
}

export const foldback = <T, U>(
    folder: BackFolder<T, U>,
    option: Option<T>,
    initial: U
) => {
    return match(
        option,
        v => folder(v, initial),
        () => initial
    )
}

export const forall = <T>(predicate: Predicate<T>, option: Option<T>) => {
    return match(option, predicate, () => true)
}

export const get = <T>(option: Option<T>) => {
    return match(
        option,
        v => v,
        () => {
            throw new Error('Cannot get value from None')
        }
    )
}

export const iter = <T>(mapper: Mapper<T, void>, option: Option<T>) => {
    match(option, mapper, () => {})
}

export const toArray = <T>(option: Option<T>) => {
    return match(
        option,
        v => [v],
        () => []
    )
}

export const toNullable = <T>(option: Option<T>) => {
    return match(
        option,
        v => v,
        () => null
    )
}
