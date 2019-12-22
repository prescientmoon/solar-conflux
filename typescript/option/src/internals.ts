export const some = Symbol('some')
export const none = Symbol('none')

export type NominalTyped<T, U> = {
    type: T
    value: U
}
