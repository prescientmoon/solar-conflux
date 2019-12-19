import { Option } from './types'

export type Mapper<T, U> = (v: T) => U
export type Binder<T, U> = Mapper<T, Option<U>>
export type Predicate<T> = Mapper<T, boolean>
export type Folder<T, U> = (s: U, v: T) => U
export type BackFolder<T, U> = (v: T, s: U) => U
