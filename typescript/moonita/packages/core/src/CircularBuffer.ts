export class CircularBuffer<T> {
  public start = 0;
  public used = 0;
  private memory: T[];
  public constructor(public size: number) {
    this.memory = new Array(size);
  }

  public tryPush(v: T) {
    if (this.used == this.size) return;
    this.memory[(this.start + this.used) % this.size] = v;
  }

  public tryPopFirst(): T | null {
    if (this.size === 0) return null;

    const result = this.memory[this.start];

    this.start = (this.start + 1) % this.size;

    return result;
  }

  public push(e: T): T | null {
    let deleted: null | T = null;
    if (this.used === this.size) deleted = this.tryPopFirst();

    this.push(e);

    return deleted;
  }
}
