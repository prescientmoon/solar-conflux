import { Fn, Nullable } from "./types";

export type Getter<T, U> = (a: T) => U[];

export const map = <A, B>(f: Fn<A, B>): Getter<A, B> => (i) => [f(i)];
export const nullable = <T>(): Getter<Nullable<T>, T> => (i) =>
  i === null ? [] : [i];

export const array = <T>(): Getter<T[], T> => (a) => a;
export const biArray = <T>(): Getter<T[][], T> =>
  emptyComposer<T[][]>().then(array()).then(array()).unwrap;

type GetterComposer<A, B> = {
  then: <C>(getter: Getter<B, C>) => GetterComposer<A, C>;
  unwrap: Getter<A, B>;
};

export const getterToComposer = <A, B>(
  getter: Getter<A, B>
): GetterComposer<A, B> => ({
  unwrap: getter,
  then: (other) => getterToComposer((i) => getter(i).flatMap(other)),
});

export const emptyComposer = <T>(): GetterComposer<T, T> =>
  getterToComposer((i) => [i]);
