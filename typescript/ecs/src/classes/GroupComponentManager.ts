import { Ecs } from './Ecs'
import { ComponentManager } from '../types/ComponentManager'
import { typesafeKeys } from '../helpers/typesafeIterable'

export class GroupComponentManager<T extends object, K extends keyof T>
  implements ComponentManager<Pick<T, K>> {
  private eids = new Set<number>()

  public constructor(
    public capacity: number,
    private ecs: Ecs<T>,
    public components: K[]
  ) {}

  private get managers() {
    return this.components.map(
      name => [this.ecs.components[name], name] as const
    )
  }

  public register(eid: number, components: Pick<T, K>) {
    for (const [manager, name] of this.managers) {
      manager.register(eid, components[name], false)
    }

    this.eids.add(eid)
  }

  public unregister(eid: number) {
    for (const [manager] of this.managers) {
      manager.unregister(eid, false)
    }

    this.eids.delete(eid)
  }

  public getComponentByEid(eid: number) {
    if (!this.eids.has(eid)) {
      throw new Error(`Cannot find component with eid ${eid}`)
    }

    const result = {} as Pick<T, K>

    for (const [manager, name] of this.managers) {
      result[name] = manager.getComponentByEid(eid)
    }

    return result
  }

  public setComponentByEid(eid: number, value: Pick<T, K>) {
    if (!this.eids.has(eid)) {
      throw new Error(`Cannot find component with eid ${eid}`)
    }

    for (const [manager, name] of this.managers) {
      manager.setComponentByEid(eid, value[name])
    }
  }

  public mutateAll(callback: (v: Pick<T, K>) => Pick<T, K>) {
    for (const eid of this.eids) {
      const original = this.getComponentByEid(eid)
      const changed = callback(original)

      this.setComponentByEid(eid, changed)
    }
  }

  public *[Symbol.iterator]() {
    for (const eid of this.eids) {
      yield this.getComponentByEid(eid)
    }
  }
}
