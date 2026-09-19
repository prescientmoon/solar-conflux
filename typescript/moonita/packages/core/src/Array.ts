/**
 * Split an array of tuples into a tuple of arrays
 */
export function splitTuples<A, B>(arr: Array<[A, B]>): [Array<A>, Array<B>] {
  return [arr.map((a) => a[0]), arr.map((a) => a[1])];
}
