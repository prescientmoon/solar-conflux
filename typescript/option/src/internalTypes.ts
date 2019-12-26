import { Option } from './types'

export type Mapper<T, U> = (v: T) => U
export type Binder<T, U> = (v: T) => Option<U>
export type Predicate<T> = (v: T) => boolean
export type Folder<T, U> = (s: U, v: T) => U
export type BackFolder<T, U> = (v: T, s: U) => U
export type Nullable<T> = T | null
export type Lazy<T> = () => T
