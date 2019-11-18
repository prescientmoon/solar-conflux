import { ComponentManager } from './../types/ComponentManager'

export class MappedComponentManager<T> implements ComponentManager<T> {
  public valueMap = new Map<number, T>()

  public register(eid: number, component: T) {
    this.valueMap.set(eid, component)
  }

  public unregister(eid: number) {
    this.valueMap.delete(eid)
  }

  public getComponentByEid(eid: number) {
    const result = this.valueMap.get(eid)

    if (result === undefined) {
      throw new Error(`Cannot find component with eid ${eid}`)
    }

    return result
  }

  public mutateAll(callback: (old: T) => T) {
    for (const [eid, value] of this.valueMap) {
      const newValue = callback(value)

      // we are checking this because in the future we might want to
      // emit an event or something when this happens
      if (newValue !== value) {
        this.valueMap.set(eid, newValue)
      }
    }
  }

  [Symbol.iterator]() {
    return this.valueMap.values()
  }
}
