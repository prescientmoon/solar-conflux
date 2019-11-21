import { Ecs } from './Ecs'
import {
  ComponentManagerBuilderMap,
  ComponentManagerBuilder,
  ExtractComponentManagerType
} from '../types/EcsHelpers'
import { GroupComponentManager } from './GroupComponentManager'

class EcsSafeBuilder<T extends object = {}> {
  public constructor(private managers: ComponentManagerBuilderMap<T>) {}

  public addManager<K extends string, L>(
    name: K,
    manager: ComponentManagerBuilder<L>
  ) {
    return new EcsSafeBuilder<T & Record<K, L>>({
      ...this.managers,
      [name]: manager
    } as any)
  }

  public group<K extends string, L extends keyof T>(
    name: K,
    components: Exclude<L, K>[]
  ) {
    const group = (ecs: Ecs<T>) =>
      new GroupComponentManager(ecs.capacity, ecs, components)

    return new EcsSafeBuilder<
      T & Record<K, ExtractComponentManagerType<ReturnType<typeof group>>>
    >({
      ...this.managers,
      [name]: group
    } as any)
  }

  public build(capacity = 10000) {
    return new Ecs(this.managers, capacity)
  }
}

export class EcsBuilder {
  public addManager<K extends string, L>(
    name: K,
    manager: ComponentManagerBuilder<L>
  ) {
    return new EcsSafeBuilder<Record<K, L>>({
      [name]: manager
    } as any)
  }
}
