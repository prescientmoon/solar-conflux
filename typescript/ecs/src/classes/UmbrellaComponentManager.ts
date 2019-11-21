import { uintType } from './../helpers/uintType'
import { ComponentManager } from './../types/ComponentManager'
import { UIntArray, typedArray } from '@thi.ng/api'

export class UmbrellaComponentManager<T> implements ComponentManager<T> {
  private sparse: UIntArray
  private dense: UIntArray

  private values: T[]
  private length = 0

  public constructor(private capacity: number) {
    const type = uintType(capacity)

    this.sparse = typedArray(type, capacity)
    this.dense = typedArray(type, capacity)

    this.values = new Array(capacity)
  }

  public register(eid: number, component: T, verboose = true) {
    const index = this.getIndex(eid)

    if (index !== undefined) {
      if (verboose) {
        throw new Error(`Component ${eid} already registered`)
      }

      return
    }

    this.values[this.length] = component

    this.sparse[eid] = this.length
    this.dense[this.length] = eid

    this.length++
  }

  public unregister(eid: number, verboose = true) {
    const index = this.getIndex(eid)

    if (index === undefined) {
      if (verboose) {
        throw new Error(`Cannot find index for component ${eid}`)
      }

      return
    }

    if (this.length > 1) {
      const last = this.length - 1
      const lastEid = this.dense[last]

      this.dense[index] = lastEid
      this.sparse[lastEid] = index

      this.values[index] = this.values[last]
    }

    this.length--
  }

  public getComponentByEid(eid: number) {
    const index = this.getIndex(eid)

    if (index === undefined) {
      throw new Error(`Cannot find component with eid ${eid}`)
    }

    return this.values[index]
  }

  public setComponentByEid(eid: number, value: T) {
    const index = this.getIndex(eid)

    if (index === undefined) {
      throw new Error(`Cannot find component with eid ${eid}`)
    }

    this.values[index] = value
  }

  private getIndex(eid: number) {
    const index = this.sparse[eid]

    if (index < this.length) {
      return index
    }

    return undefined
  }

  public [Symbol.iterator]() {
    return this.values.slice(0, this.length)[Symbol.iterator]()
  }

  public mutateAll(callback: (v: T) => T) {
    for (let index = this.length; index-- > 0; ) {
      this.values[index] = callback(this.values[index])
    }
  }
}
