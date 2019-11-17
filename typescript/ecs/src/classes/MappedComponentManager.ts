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

  [Symbol.iterator]() {
    return this.valueMap.values()
  }
}
