import { Lazy } from "./types";

export const replicate = <T>(element: Lazy<T>, size: number) =>
  Array(size)
    .fill(1)
    .map((_) => element());
