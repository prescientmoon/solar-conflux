import { ComponentManager } from '../types/ComponentManager'
import { Type, TypedArray, typedArray, UIntArray } from '@thi.ng/api'
import { Ecs } from './Ecs'
import { uintType } from '../helpers/uintType'

export class VectorComponentManager implements ComponentManager<number> {
  private static memory = {} as Record<Type, TypedArray>
  private static offsets = {} as Record<Type, number>

  public static clear() {
    this.memory = {} as Record<Type, TypedArray>
    this.offsets = {} as Record<Type, number>
  }

  private offset = 0
  private capacity = 0

  private sparse: UIntArray
  private dense: UIntArray

  private length = 0
  private values: TypedArray

  public constructor(private type: Type = Type.F64) {}

  public init(ecs: Ecs<any>) {
    this.capacity = ecs.capacity

    const type = uintType(ecs.capacity)

    this.sparse = typedArray(type, ecs.capacity)
    this.dense = typedArray(type, ecs.capacity)

    if (!VectorComponentManager.memory[this.type]) {
      const sameType = ecs.managers.filter(
        manager =>
          manager instanceof VectorComponentManager &&
          manager.type === this.type
      )

      const length = ecs.capacity * sameType.length

      VectorComponentManager.memory[this.type] = typedArray(this.type, length)
      VectorComponentManager.offsets[this.type] = 0
    } else {
      this.offset = ++VectorComponentManager.offsets[this.type]
    }

    this.values = VectorComponentManager.memory[this.type]
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

      this.values[index + this.offset] = this.values[last + this.offset]
    }

    this.length--
  }

  public mutateAll(callback: (v: number) => number) {
    for (let i = 0; i < this.capacity; i++) {
      VectorComponentManager.memory[this.type][i + this.offset] = callback(
        VectorComponentManager.memory[this.type][i + this.offset]
      )
    }
  }

  private getIndex(eid: number) {
    const index = this.sparse[eid]

    if (index < this.length && this.dense[index] === eid) {
      return index
    }

    return undefined
  }

  public register(eid: number, component: number, verboose = true) {
    const index = this.getIndex(eid)

    if (index !== undefined) {
      if (verboose) {
        throw new Error(`Component ${eid} already registered`)
      }

      return
    }

    this.values[this.offset + this.length] = component

    this.sparse[eid] = this.length
    this.dense[this.length] = eid

    this.length++
  }

  public getComponentByEid(eid: number) {
    const index = this.getIndex(eid)

    if (index === undefined) {
      throw new Error(`Cannot find component with eid ${eid}`)
    }

    return this.values[this.offset + index]
  }

  public setComponentByEid(eid: number, value: number) {
    const index = this.getIndex(eid)

    if (index === undefined) {
      throw new Error(`Cannot find component with eid ${eid}`)
    }

    this.values[this.offset + index] = value
  }

  public [Symbol.iterator]() {
    return this.values
      .slice(this.offset, this.offset + this.length)
      [Symbol.iterator]()
  }
}
