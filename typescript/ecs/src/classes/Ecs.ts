import { System } from './../types/System'
import { EidGenerator } from './EidGenerator'
import { ComponentManager } from '../types/ComponentManager'
import { MappedComponentManager } from './MappedComponentManager'

type ComponentManagerClassMap<T extends object> = {
  [K in keyof T]: { new (capacity: number): ComponentManager<T[K]> }
}

type ComponentManagerMap<T extends object> = {
  [K in keyof T]: ComponentManager<T[K]>
}

type ComponentList<T> = (keyof T)[]
type SystemMap<T extends object> = { [K in keyof T]: System<T[K]>[] }

export class Ecs<T extends object> {
  private componentLists = new MappedComponentManager<ComponentList<T>>()
  private systems = {} as SystemMap<T>
  public components = {} as ComponentManagerMap<T>

  public constructor(
    components: ComponentManagerClassMap<T>,
    public capacity = 10000,
    private generator = new EidGenerator()
  ) {
    for (const [key, Component] of Object.entries(components)) {
      this.components[
        key
      ] = new (Component as typeof components[keyof typeof components])(
        capacity
      )
    }

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

    // perform didCreate hook
    for (const name in components) {
      const typedName = name as keyof typeof components

      for (const system of this.systems[typedName]) {
        if (system.didCreate) {
          system.didCreate!(components[typedName]!, eid)
        }
      }
    }

    this.componentLists.register(
      eid,
      Object.keys(components) as ComponentList<T>
    )

    return eid
  }

  public destroy(...eids: number[]) {
    for (const eid of eids) {
      const components = this.componentLists.getComponentByEid(eid)

      for (const componentName of components) {
        const component = this.components[componentName].getComponentByEid(eid)
        this.components[componentName].unregister(eid)

        for (const system of this.systems[componentName]) {
          if (system.didDestroy) {
            system.didDestroy(component, eid)
          }
        }
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
      const hasBeforeUpdate = this.systems[componentName].filter(
        system => system.beforeUpdate
      )

      const needUpdate = this.systems[componentName].filter(
        system => system.onUpdate
      )

      const hasDidUpdate = this.systems[componentName].filter(
        system => system.didUpdate
      )

      const componentManager = this.components[componentName]

      // We don't need to iterate over everything if we don't need the mutations
      if (needUpdate.length) {
        componentManager.mutateAll(oldComponent => {
          let nextComponentValue = oldComponent

          for (const system of needUpdate) {
            const result = system.onUpdate!(oldComponent)

            if (result !== undefined) {
              nextComponentValue = result
            }
          }

          return nextComponentValue
        })
      }

      // run didUpdate hook
      for (const system of hasDidUpdate) {
        system.didUpdate!(componentManager)
      }
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
