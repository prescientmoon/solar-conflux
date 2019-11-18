import { System } from './System'
import { EidGenerator } from './EidGenerator'
import { ComponentManager } from '../types/ComponentManager'
import { MappedComponentManager } from './MappedComponentManager'

type ComponentManagerMap<T extends object> = {
  [K in keyof T]: ComponentManager<T[K]>
}

type ComponentList<T> = (keyof T)[]

type SystemMap<T extends object> = { [K in keyof T]: System<T[K]>[] }

export class Ecs<T extends object> {
  private componentLists = new MappedComponentManager<ComponentList<T>>()
  private systems = {} as SystemMap<T>

  public constructor(
    public components: ComponentManagerMap<T>,
    private generator = new EidGenerator()
  ) {
    for (const key in components) {
      this.systems[key] = []
    }
  }

  public create(components: Partial<T>) {
    const eid = this.generator.create()

    for (const name in components) {
      const typedName = name as keyof typeof components

      this.components[typedName].register(eid, components[typedName]!)
    }

    // notify components
    for (const name in components) {
      for (const system of this.systems[name as keyof typeof components]) {
        system.didCreate(components[name]!)
      }
    }

    this.componentLists.register(
      eid,
      Object.keys(components) as ComponentList<T>
    )

    return eid
  }

  public registerSystem<K extends keyof T>(
    SystemClass: {
      new (componentManager: ComponentManager<T[K]>): System<T[K]>
    },
    name: K
  ) {
    this.systems[name].push(new SystemClass(this.components[name]))
  }

  public getComponentsByEid<K extends keyof T>(eid: number) {
    const baseObject = {} as Pick<T, K>

    for (const key of this.componentLists.getComponentByEid(eid)) {
      const typedKey = key as K

      baseObject[typedKey] = this.components[typedKey].getComponentByEid(eid)
    }

    return baseObject
  }

  public pickComponentByEid<K extends keyof T>(eid: number, components: K[]) {
    const baseObject = {} as Pick<T, K>

    for (const key of components) {
      const typedKey = key as K

      baseObject[typedKey] = this.components[typedKey].getComponentByEid(eid)
    }

    return baseObject
  }
}
