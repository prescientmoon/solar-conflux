import { System } from './System'
import { ComponentManager } from './ComponentManager'

export type ComponentManagerMap<T extends object> = {
  [K in keyof T]: ComponentManager<T[K]>
}

export type ComponentList<T> = (keyof T)[]
export type SystemMap<T extends object> = { [K in keyof T]: System<T[K]>[] }

export type ExtractComponentManagerType<M> = M extends ComponentManager<infer R>
  ? R
  : never
