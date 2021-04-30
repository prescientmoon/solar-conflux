import { array } from "./iterate";
import { Lazy } from "./types";

export const replicate = <T>(element: Lazy<T>, size: number) =>
  Array(size)
    .fill(1)
    .map((_) => element());

/** Removes index from array, doesn't preserve ordering */
export const removeIndex = <T>(array: T[], index: number) => {
  if (array.length === 0) return;
  if (array.length === 1 && index === 0) array.splice(0, 1);
  else {
    array[index] = array.pop()!;
  }
};

/** Generator for the reverse of an array */
export function* reversed<T>(array: T[]) {
  for (let index = array.length - 1; index >= 0; index--) {
    yield array[index];
  }
}
