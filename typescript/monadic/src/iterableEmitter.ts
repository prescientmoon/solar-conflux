// An event emitter which can be used
// with for of loops
export class IterableEmitter<T> {
  // The generation of the emitter
  // Used to avoid trying to resolve a promise twice
  private generation = 0;

  // Set the state
  public next = (_: T) => {};

  public constructor(private state: T) {}

  async *[Symbol.asyncIterator](): AsyncGenerator<T> {
    const createPromise = () =>
      new Promise<T>(resolve => {
        const generation = this.generation;

        this.next = (value: T) => {
          if (generation !== this.generation) {
            throw new Error('Cannot resolve the same generation twice');
          }

          this.generation++;
          resolve(value);
        };
      });

    yield this.state;

    while (true) {
      yield createPromise();
    }
  }
}
