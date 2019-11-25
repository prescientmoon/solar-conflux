import { rangeArray } from './rangeArray'

export const chunkX = <T>(arr: T[], width: number, height: number): T[][] =>
    rangeArray(0, this.height).map(index =>
        arr.slice(index * height, (index + 1) * width)
    )
