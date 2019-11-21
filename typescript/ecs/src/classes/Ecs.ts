import { ComponentManagerClass } from './../types/ComponentManager'
import {
  SystemMap,
  ComponentManagerMap,
  ComponentList,
  ComponentManagerBuilderMap,
  FunctionalManagerBuilder
} from '../types/EcsHelpers'
import { System } from './../types/System'
import { EidGenerator } from './EidGenerator'
import { ComponentManager } from '../types/ComponentManager'
import { MappedComponentManager } from './MappedComponentManager'
import {
  typesafeIterable,
  typesafeKeys,
  typesafeEntries
} from '../helpers/typesafeIterable'
import { withProp } from '../helpers/whichHaveProp'

export class Ecs<T extends object = {}> {
  private componentLists = new MappedComponentManager<ComponentList<T>>()
  private systems = {} as SystemMap<T>
  public components = {} as ComponentManagerMap<T>

  public constructor(
    components: ComponentManagerBuilderMap<T>,
    public capacity = 10000,
    private generator = new EidGenerator()
  ) {
    for (const [key, Component] of typesafeEntries(components)) {
      try {
        this.components[key] = (Component as FunctionalManagerBuilder<
          T[keyof T]
        >)(this)
      } catch {
        this.components[key] = new (Component as ComponentManagerClass<
          T[keyof T]
        >)(capacity)
      }
    }

    for (const key in components) {
      this.systems[key] = []
    }
  }

  public create(components: Partial<Pick<T, keyof T>>) {
    const eid = this.generator.create()

    // perform beforeCreate hook
    for (const name of typesafeIterable<keyof T>(Object.keys(components)))
      for (const system of withProp(this.systems[name], 'beforeCreate')) {
        const result = system.beforeCreate(components[name]!)

        if (result === false) {
          this.generator.destroy(eid)

          return eid
        }
      }

    for (const name of typesafeKeys(components)) {
      let component = components[name]!

      // perform onUpdate hook
      for (const system of withProp(this.systems[name], 'onCreate')) {
        const result = system.onCreate(component)

        if (result !== undefined) {
          component = result as typeof component
        }
      }

      this.components[name].register(eid, component)
    }

    // perform didCreate hook
    for (const name of typesafeKeys(components)) {
      for (const system of withProp(this.systems[name], 'didCreate')) {
        system.didCreate(components[name]!, eid)
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

    return this
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

      // run beforeUpdate hook
      for (const system of hasBeforeUpdate) {
        system.beforeUpdate!(componentManager)
      }

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

      for (const system of withProp(this.systems[componentName], 'onRender')) {
        for (const component of componentManager) {
          system.onRender(component)
        }
      }
    }
  }
}
