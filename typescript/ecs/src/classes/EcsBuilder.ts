import { Ecs } from './Ecs'
import { ExtractComponentManagerType } from '../types/EcsHelpers'
import { GroupComponentManager } from './GroupComponentManager'
import {
  ComponentManagerBuilderMap,
  ComponentManagerBuilder
} from '../types/ComponentBuilder'

export class EcsBuilder {
  public constructor(protected readonly capacity = 1000) {}

  public addManager<K extends string, L>(
    name: K,
    manager: ComponentManagerBuilder<L>
  ) {
    return new EcsSafeBuilder<Record<K, L>>(
      {
        [name]: manager
      } as any,
      this.capacity
    )
  }

  public setCapacity(capacity: number) {
    return new EcsBuilder(capacity)
  }
}

class EcsSafeBuilder<T extends object = {}> {
  public constructor(
    private readonly managers: ComponentManagerBuilderMap<T>,
    private readonly capacity
  ) {}

  public addManager<K extends string, L>(
    name: K,
    manager: ComponentManagerBuilder<L>
  ) {
    return new EcsSafeBuilder<T & Record<K, L>>(
      {
        ...this.managers,
        [name]: manager
      } as any,
      this.capacity
    )
  }

  public group<K extends string, L extends keyof T>(
    name: K,
    components: Exclude<L, K>[]
  ) {
    const group = (ecs: Ecs<T>) =>
      new GroupComponentManager(ecs.capacity, ecs, components)

    return new EcsSafeBuilder<
      T & Record<K, ExtractComponentManagerType<ReturnType<typeof group>>>
    >(
      {
        ...this.managers,
        [name]: group
      } as any,
      this.capacity
    )
  }

  public setCapacity(capacity: number) {
    return new EcsSafeBuilder(this.managers, capacity)
  }

  public build() {
    return new Ecs(this.managers, this.capacity)
  }
}
