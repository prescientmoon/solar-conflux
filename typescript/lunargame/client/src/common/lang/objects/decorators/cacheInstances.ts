import { decorable } from '@eix/utils'
import { areEqual } from '../helpers/areEqual'

export interface ObjectArgumentsRef<T> {
    instance: T
    arguments: unknown[]
}

export const cacheInstances = (argCount = Infinity) => {
    const objectMemory: ObjectArgumentsRef<unknown>[] = []

    return <T extends { new (...args: any[]): { init: () => void } }>(
        toDecorate: T
    ) => {
        return class extends toDecorate {
            constructor(...args: any[]) {
                super(...args)

                const sliceParameters =
                    argCount === Infinity ? [0] : [0, argCount]
                const argumentsToStore = args.slice(...sliceParameters)

                const reference = objectMemory.find(instance =>
                    areEqual(argumentsToStore, instance.arguments)
                )

                if (reference) return reference.instance as this
                else
                    objectMemory.push({
                        instance: this,
                        arguments: argumentsToStore
                    })

                if (super.init) super.init()
            }
        }
    }
}
