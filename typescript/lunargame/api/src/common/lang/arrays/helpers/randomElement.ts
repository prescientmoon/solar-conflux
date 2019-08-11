/**
 * Returns a random element from an array
 *
 * @param arr The array to select the element from
 * @throws Error if the array has length 0
 */
export const randomElement = <T>(arr: T[]): T => {
    if (!arr.length) {
        throw new Error('Cannot choose a random element from array of length 0')
    }

    return arr[Math.floor(arr.length * Math.random())]
}
