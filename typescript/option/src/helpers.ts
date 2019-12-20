import { Option, Some, None } from './types'
import {
    Binder,
    Folder,
    Mapper,
    Predicate,
    BackFolder,
    Nullable
} from './internalTypes'
import { always, identity } from './internalHelperts'
import Internals, { SomeClass, isOption } from './internals'

export const isSome = <T>(option: Option<T>) =>
    option instanceof Internals.SomeClass
export const isNothing = <T>(option: Option<T>) =>
    option instanceof Internals.NoneClass

export const match = <T, U>(
    option: Option<T>,
    caseSome: Mapper<T, U>,
    caseNone: Mapper<void, U>
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
    return match(option, binder, always(None))
}

export const map = <T, U>(
    mapper: Mapper<T, U>,
    option: Option<T>
): Option<U> => {
    return match(option, v => Some(mapper(v)), always(None))
}

export const count = <T>(option: Option<T>) => Number(isSome(option))

export const exists = <T>(predicate: Predicate<T>, option: Option<T>) => {
    return match(option, predicate, always(false))
}

export const filter = <T>(predicate: Predicate<T>, option: Option<T>) => {
    return match(option, v => (predicate(v) ? Some(v) : None), always(None))
}

export const fold = <T, U>(
    folder: Folder<T, U>,
    initial: U,
    option: Option<T>
) => {
    return match(option, v => folder(initial, v), always(initial))
}

export const foldback = <T, U>(
    folder: BackFolder<T, U>,
    option: Option<T>,
    initial: U
) => {
    return match(option, v => folder(v, initial), always(initial))
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
    return match(option, mapper, always(None))
}

export const toArray = <T>(option: Option<T>) => {
    return match(option, v => [v], always([]))
}

export const toNullable = <T>(option: Option<T>) => {
    return match(option, identity, always(null))
}

export const withDefault = <T>(_default: T, option: Option<T>) => {
    return match(option, identity, always(_default))
}

const checkIfOption = <T>(x): x is Option<T> => x[isOption]

export const flat = <T, U>(option: Option<T>): Option<U> => {
    return match(
        option,
        inner => {
            if (checkIfOption(inner)) {
                return flat(inner)
            } else {
                return Some(inner)
            }
        },
        always(None)
    )
}

export const fromNullable = <T>(value: Nullable<T>): Option<T> => {
    return value === null ? None : Some(value)
}
