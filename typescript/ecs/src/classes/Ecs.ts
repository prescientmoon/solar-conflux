import { System } from './../types/System'
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

    this.componentLists.register(
      eid,
      Object.keys(components) as ComponentList<T>
    )

    return eid
  }

  public destroy(...eids: number[]) {
    for (const eid of eids) {
      for (const componentName of this.componentLists.getComponentByEid(eid)) {
        this.components[componentName].unregister(eid)
      }
    }
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

  public update() {
    for (const componentName in this.systems) {
      const componentManager = this.components[componentName]

      const needUpdate = this.systems[componentName].filter(
        system => system.onUpdate
      )

      // We don't need to iterate over everything if we don't need the mutations
      if (!needUpdate.length) {
        return
      }

      componentManager.mutateAll(oldComponent => {
        let nextComponentValue = oldComponent

        const setComponent = (newComponent: T[typeof componentName]) => {
          nextComponentValue = newComponent
        }

        for (const system of needUpdate) {
          system.onUpdate!(oldComponent, setComponent)
        }

        return nextComponentValue
      })
    }
  }

  public render() {
    for (const componentName in this.systems) {
      const componentManager = this.components[componentName]

      const needRender = this.systems[componentName].filter(
        system => system.onRender
      )

      // We don't need to iterate over everything if we don't need the mutations
      if (!needRender.length) {
        return
      }

      for (const system of needRender) {
        for (const component of componentManager) {
          system.onRender!(component)
        }
      }
    }
  }
}
