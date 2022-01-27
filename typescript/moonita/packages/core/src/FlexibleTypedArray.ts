import { TypedArray, TypedArrayConstructor } from "wolf-ecs";

export interface IFlexibleTypedArray<T> {
  used: number;
  readonly capacity: number;
  /** Attempt to push an element. Errors on failure. */
  push(e: T): void;
  /** Attempts to push an element. Fails silently */
  tryPush(e: T): void;
  /** Attempts to remove the last element in the array and return it.
   * Returns null if the array is empty
   */
  tryPop(): T | null;
  /** Delete every element in the array */
  clear(): void;
  /** Sets the element at a given index. Does not perform and safety checks. */
  set(i: number, e: T): void;
  /** Returns the element at the given index */
  get(i: number): T | null;
}

/** TypedArray that keeps track of it's own size
 * TODO: implement dynamic capacity (I'll do this once I run into an usecase where I need it)
 */
export class FlexibleTypedArray<T extends number | bigint = number>
  implements IFlexibleTypedArray<T>
{
  public elements: TypedArray & Record<number, T>;
  public used = 0;

  public constructor(
    public readonly capacity: number,
    wrapAround: TypedArrayConstructor
  ) {
    this.elements = new wrapAround(this.capacity) as any;
  }

  public tryPush(e: T) {
    if (this.capacity === this.used) return;

    this.elements[this.used] = e;
    this.used++;
  }

  public push(e: T) {
    if (this.capacity === this.used)
      throw new Error(`Cannot push new element ${e}. Array already full`);

    this.tryPush(e);
  }

  public tryPop(): T | null {
    if (this.used === 0) return null;

    this.used--;
    return this.elements[this.used];
  }

  public clear() {
    this.used = 0;
  }

  public get(i: number) {
    if (i < 0 || i >= this.used) return null;
    return this.elements[i];
  }

  public set(i: number, value: T) {
    this.elements[i] = value;
  }
}

export class FlexibleTypedArraySlice<T extends number | bigint = number>
  implements IFlexibleTypedArray<T>
{
  public used = 0;

  public constructor(
    public readonly start: number,
    public readonly capacity: number,
    public readonly source: IFlexibleTypedArray<T>
  ) {}

  public tryPush(e: T) {
    if (this.capacity === this.used) return;

    this.source.set(this.used + this.start, e);
    this.used++;
  }

  public push(e: T) {
    if (this.capacity === this.used)
      throw new Error(`Cannot push new element ${e}. Array already full`);

    this.tryPush(e);
  }

  public tryPop(): T | null {
    if (this.used === 0) return null;

    this.used--;
    return this.source.get(this.start + this.used);
  }

  public clear() {
    this.used = 0;
  }

  public get(i: number) {
    if (i < 0 || i >= this.used) return null;
    return this.source.get(i + this.start);
  }

  public set(i: number, v: T) {
    this.source.set(i + this.start, v);
  }
}
