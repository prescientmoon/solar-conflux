import { ComponentManager } from './ComponentManager'

export interface System<T> {
  beforeUpdate?(manager: ComponentManager<T>): boolean | void
  beforeRender?(manager: ComponentManager<T>): boolean | void
  beforeCreate?(component: T): boolean | void

  onCreate?(component: T): T | void
  onUpdate?(component: T): T | void
  onRender?(component: T): void

  didUpdate?(manager: ComponentManager<T>): void
  didRender?(manager: ComponentManager<T>): void
  didCreate?(component: T, eid: number): void
  didDestroy?(component: T, eid: number): void
}
