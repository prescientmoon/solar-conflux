import { ComponentManager } from './../types/ComponentManager'

export interface System<T> {
  // implemented
  beforeUpdate?(manager: ComponentManager<T>): void

  // implemented
  onUpdate?(component: T): T | void

  // implemented
  onRender?(component: T): void

  // implemented
  didUpdate?(manager: ComponentManager<T>): void

  // implemented
  didCreate?(component: T, eid: number): void

  // implemented
  didDestroy?(component: T, eid: number): void

  // not implemented
  beforeCreate?(component: T): T | void
}
