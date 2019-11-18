import { ComponentManager } from './../types/ComponentManager'

export interface System<T> {
  onUpdate?(component: T, setComponent: (value: T) => void): void
  onRender?(component: T): void
}
