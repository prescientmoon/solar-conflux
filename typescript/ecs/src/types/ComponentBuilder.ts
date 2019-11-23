import { ComponentManager } from './ComponentManager'
import { Ecs } from '../classes/Ecs'

export type ComponentManagerClass<T> = {
  new (capacity: number): ComponentManager<T>
}

export type FunctionalManagerBuilder<T> = (ecs: Ecs<any>) => ComponentManager<T>

export type ComponentManagerBuilder<T> =
  | FunctionalManagerBuilder<T>
  | ComponentManagerClass<T>

export type ComponentManagerBuilderMap<T extends object> = {
  [K in keyof T]: ComponentManagerBuilder<T[K]>
}
