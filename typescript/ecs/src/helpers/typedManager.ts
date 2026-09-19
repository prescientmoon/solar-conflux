import { ComponentManagerClass } from '../types/ComponentManager'

export const typedManager = <T>(manager: ComponentManagerClass<T>) => manager

export type typedManager<T> = ComponentManagerClass<T>
