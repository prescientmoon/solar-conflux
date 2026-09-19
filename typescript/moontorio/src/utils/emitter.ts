export type Listener<T> = (ev: T) => void;

// Very basic typed event emitter
export class EventEmitter<T> {
  private listeners: Partial<{ [K in keyof T]: Listener<T[K]>[] }> = {};

  public on<K extends keyof T>(name: K, listener: Listener<T[K]>) {
    if (!this.listeners[name]) this.listeners[name] = [];

    this.listeners[name]!.push(listener);
  }

  public emit<K extends keyof T>(name: K, data: T[K]) {
    if (!this.listeners[name]) return;

    for (const listener of this.listeners[name] as Listener<T[K]>[]) {
      listener(data);
    }
  }
}
