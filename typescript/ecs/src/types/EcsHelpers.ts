import { Ecs } from '../classes/Ecs'
import { System } from './System'
import { ComponentManager, ComponentManagerClass } from './ComponentManager'

export type FunctionalManagerBuilder<T> = (ecs: Ecs<any>) => ComponentManager<T>

export type ComponentManagerBuilder<T> =
  | FunctionalManagerBuilder<T>
  | ComponentManagerClass<T>

export type ComponentManagerBuilderMap<T extends object> = {
  [K in keyof T]: ComponentManagerBuilder<T[K]>
}

export type ComponentManagerMap<T extends object> = {
  [K in keyof T]: ComponentManager<T[K]>
}

export type ComponentList<T> = (keyof T)[]
export type SystemMap<T extends object> = { [K in keyof T]: System<T[K]>[] }

export type ExtractComponentManagerType<M> = M extends ComponentManager<infer R>
  ? R
  : never
