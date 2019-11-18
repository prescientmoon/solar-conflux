export interface ComponentManager<T> {
  register(eid: number, compoennt: T): void
  unregister(eid: number): void
  getComponentByEid(eid: number): T
  mutateAll(callback: (value: T) => T): void

  [Symbol.iterator](): Iterable<T>
}
