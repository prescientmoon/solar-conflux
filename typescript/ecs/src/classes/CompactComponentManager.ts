import { ComponentManager } from '../types/ComponentManager'
import { DoubleMap } from './DoubleMap'

export class CompactComponentManager<T> implements ComponentManager<T> {
  public components: T[] = []

  public eidsToIndices = new DoubleMap<number, number>()

  public register(eid: number, component: T) {
    if (this.eidsToIndices.getFromHead(eid) !== undefined) {
      throw new Error(`Component with eid ${eid} already registered`)
    }

    const index = this.components.push(component) - 1

    this.eidsToIndices.set(eid, index)
  }

  public unregister(eid: number) {
    const index = this.eidsToIndices.getFromHead(eid)

    if (index === undefined) {
      throw new Error(`Entity ${eid} was never registered`)
    }

    this.eidsToIndices.deleteByHead(eid)

    // We are sure this exists because the index is 0
    const lastComponent = this.components.pop()!

    if (this.components[index] === undefined) {
      return
    }

    this.components[index] = lastComponent

    const movedIndex = this.components.length
    const movedEid = this.eidsToIndices.getFromTail(movedIndex)!

    this.eidsToIndices.set(movedEid, index)
  }

  public getComponentByEid(eid: number) {
    const index = this.eidsToIndices.getFromHead(eid)

    if (index === undefined) {
      throw new Error(`Cannot find component with eid: ${eid}`)
    }

    return this.components[index]
  }

  public mutateAll(callback: (old: T) => T) {
    for (let index = 0; index < this.components.length; index++) {
      const oldComponent = this.components[index]
      const newComponent = callback(oldComponent)

      // we are checking this because in the future we might want to
      // emit an event or something when this happens
      if (newComponent !== oldComponent) {
        this.components[index] = newComponent
      }
    }
  }

  [Symbol.iterator]() {
    return this.components[Symbol.iterator]()
  }
}
