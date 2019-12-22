export const some = 'some'
export const none = 'none'

export type NominalTyped<T, U> = {
    _type: T
    value: U
}
