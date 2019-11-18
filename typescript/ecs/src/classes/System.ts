import { key } from './../types/key'
import { ComponentManager } from './../types/ComponentManager'
import { Ecs } from './Ecs'

export class System<T> {
  public constructor(private componentManager: ComponentManager<T>) {}

  public didCreate(component: T) {}
}
