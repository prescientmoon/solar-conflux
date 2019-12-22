import { Option, Some, None } from './types'
import {
    Binder,
    Folder,
    Mapper,
    Predicate,
    BackFolder,
    Nullable
} from './internalTypes'
import { identity, none, some } from './internals'

export const isSome = <T>(option: Option<T>) => option._type === some
export const isNothing = <T>(option: Option<T>) => option._type === none

const match = <T, U>(
    caseSome: Mapper<T, U>,
    _default: U,
    option: Option<T>
) => {
    if (option._type === some) {
        return caseSome(option.value)
    }

    return _default
}

export const bind = <T, U>(
    binder: Binder<T, U>,
    option: Option<T>
): Option<U> => {
    return match(binder, None, option)
}

export const map = <T, U>(
    mapper: Mapper<T, U>,
    option: Option<T>
): Option<U> => {
    return match(v => Some(mapper(v)), None, option)
}

export const count = <T>(option: Option<T>) => Number(isSome(option))

export const exists = <T>(predicate: Predicate<T>, option: Option<T>) => {
    return match(predicate, false, option)
}

export const filter = <T>(predicate: Predicate<T>, option: Option<T>) => {
    return match(v => (predicate(v) ? Some(v) : None), None, option)
}

export const fold = <T, U>(
    folder: Folder<T, U>,
    initial: U,
    option: Option<T>
) => {
    return match(v => folder(initial, v), initial, option)
}

export const foldback = <T, U>(
    folder: BackFolder<T, U>,
    option: Option<T>,
    initial: U
) => {
    return match(v => folder(v, initial), initial, option)
}

export const forall = <T>(predicate: Predicate<T>, option: Option<T>) => {
    return match(predicate, true, option)
}

export const get = <T>(option: Option<T>): T => {
    if (option._type === some) {
        return option.value
    }

    throw new Error(`Cannot get value of None`)
}

export const iter = <T>(mapper: Mapper<T, void>, option: Option<T>) => {
    if (option._type === some) {
        mapper(option.value)
    }
}

export const toArray = <T>(option: Option<T>) => {
    return match(v => [v], [], option)
}

export const toNullable = <T>(option: Option<T>) => {
    return match(identity, null, option)
}

export const withDefault = <T>(_default: T, option: Option<T>) => {
    return match(identity, _default, option)
}

export const flat = <T>(option: Option<Option<T>>): Option<T> => {
    return bind(identity, option)
}

export const fromNullable = <T>(value: Nullable<T>): Option<T> => {
    return value === null ? None : Some(value)
}

export const fromArray = <T>(value: [T] | []): Option<T> => {
    return value[0] === undefined ? None : Some(value[0])
}
