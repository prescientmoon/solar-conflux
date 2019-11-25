import { assert } from '@thi.ng/api'
import { List } from 'immutable'
import { Direction } from '../types/direction'
import { rangeArray } from '../helpers/rangeArray'

export class GameState {
    public height: number

    public constructor(public cells: List<number>, public width: number) {
        const height = cells.count() / width

        assert(
            height === Math.floor(height),
            `Recived non-integer height: ${height}`
        )

        this.height = height
    }

    public moveX(direction: Direction, layers: Iterable<number>) {
        const newState = this.cells.withMutations(list => {
            for (const layer of layers) {
                const slice = list.slice(
                    layer * this.width,
                    (layer + 1) * this.width
                )

                if (direction === -1) {
                    const first = slice.first<number>()
                    slice.shift()
                    slice.push(first)
                } else if (direction === 1) {
                    const last = slice.last<number>()
                    slice.unshift(last)
                }

                for (let i = 0; i < slice.count(); i++) {
                    list[i + layer * this.width] = slice[i]
                }
            }
        })

        return new GameState(newState, this.width)
    }
}
