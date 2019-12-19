export const isOption = Symbol('option')
export const someValue = Symbol('value')

export class SomeClass<T> {
    public [isOption] = true
    public [someValue]: T

    public constructor(value: T) {
        this[someValue] = value
    }
}

export class NoneClass {
    public [isOption] = true
}

export default { NoneClass, SomeClass, isOption, someValue }
