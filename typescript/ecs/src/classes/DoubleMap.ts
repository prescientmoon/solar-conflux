export class DoubleMap<T, K> {
  public head = new Map<T, K>()
  public tail = new Map<K, T>()

  public set(headValue: T, tailValue: K) {
    this.deleteByHead(headValue)
    this.deleteByTail(tailValue)

    this.head.set(headValue, tailValue)
    this.tail.set(tailValue, headValue)
  }

  public getFromHead(headValue: T) {
    return this.head.get(headValue)
  }

  public getFromTail(tailValue: K) {
    return this.tail.get(tailValue)
  }

  public deleteByHead(headValue: T) {
    const tailValue = this.getFromHead(headValue)

    if (tailValue === undefined) {
      return
    }

    this.head.delete(headValue)
    this.tail.delete(tailValue)
  }

  public deleteByTail(tailValue: K) {
    const headValue = this.getFromTail(tailValue)

    if (headValue === undefined) {
      return
    }

    this.head.delete(headValue)
    this.tail.delete(tailValue)
  }
}
