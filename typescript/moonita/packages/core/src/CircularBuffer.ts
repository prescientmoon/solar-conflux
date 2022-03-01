export class CircularBuffer<T> {
  public start = 0;
  public used = 0;
  private memory: T[];
  public constructor(public size: number) {
    this.memory = new Array(size);
  }

  public toLocalIndex(index: number) {
    return (index + this.start) % this.size;
  }

  /** Get the nth element of the buffer */
  public get(index: number): T | null {
    if (index >= this.used) return null;

    const rawIndex = this.toLocalIndex(index);

    return this.memory[rawIndex];
  }

  public set(index: number, v: T) {
    if (index >= this.used)
      throw new Error(
        `Cannot set index ${index} to value ${v} because the index is out of range`
      );

    const raw = this.toLocalIndex(index);
    this.memory[raw] = v;
  }

  public tryPush(v: T) {
    if (this.used == this.size) return;
    this.memory[(this.start + this.used) % this.size] = v;
    this.used++;
  }

  public remove(index: number) {
    this.swap(0, index);
    this.tryPopFirst();
  }

  public swap(a: number, b: number) {
    if (a === b) return;

    const c = this.get(a);
    const d = this.get(b);

    if (c === null || d === null)
      throw new Error(`One of the indices ${a} and ${b} is out of bounds`);

    this.set(a, d);
    this.set(b, c);
  }

  public tryPopFirst(): T | null {
    if (this.size === 0) return null;

    const result = this.memory[this.start];

    this.memory[this.start] = null as any;

    this.start = (this.start + 1) % this.size;
    this.used--;

    return result;
  }

  public pushMany(elements: T[]) {
    for (let i = 0; i < elements.length; i++) {
      this.push(elements[i]);
    }
  }

  public push(e: T): T | null {
    let deleted: null | T = null;
    if (this.used === this.size) deleted = this.tryPopFirst();

    this.tryPush(e);

    return deleted;
  }

  /** Save all the elements in the buffer as an array */
  public toArray(): T[] {
    const arr: T[] = [];

    for (let i = 0; i < this.used; i++) arr.push(this.get(i)!);

    return arr;
  }

  /** Delete all the elements in the buffer */
  public clear() {
    for (let i = 0; i < this.used; i++) {
      this.set(i, null as any);
    }

    this.start = 0;
    this.used = 0;
  }
}
