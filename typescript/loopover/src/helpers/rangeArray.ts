export const rangeArray = (start: number, end: number) =>
    Array(end - start)
        .fill(true)
        .map((_, i) => i + start)
