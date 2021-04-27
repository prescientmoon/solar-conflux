export type Nullable<T> = T | null;
export type Vec2 = [number, number];
export type ADT<T> = {
  [K in keyof T]: {
    type: K;
  } & T[K];
}[keyof T];

export type Pair<T> = [T, T];
export type Fn<A, B> = (input: A) => B;
export type Lazy<T> = () => T;

export const enum Direction {
  Right,
  Down,
  Left,
  Up,
}

export type Neighbour = Pair<-1 | 0 | 1>;
