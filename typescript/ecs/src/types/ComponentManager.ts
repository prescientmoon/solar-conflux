export interface ComponentManager<T> {
  register(eid: number, compoennt: T): void
  unregister(eid: number): void
  getComponentByEid(eid: number): T

  [Symbol.iterator](): Iterable<T>
}
