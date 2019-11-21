export interface ComponentManager<T> {
  register(eid: number, compoennt: T, verboose?: boolean): void
  unregister(eid: number, verboose?: boolean): void
  getComponentByEid(eid: number): T
  setComponentByEid(eid: number, value: T): void
  mutateAll(callback: (value: T) => T): void

  [Symbol.iterator](): IterableIterator<T>
}

export type ComponentManagerClass<T> = {
  new (capacity: number): ComponentManager<T>
}
