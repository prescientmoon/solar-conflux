import { assert } from '@thi.ng/api'
import { List } from 'immutable'
import { Direction } from '../types/direction'

export class GameState {
    public height: number

    public constructor(private cellList: List<number>, public width: number) {
        const height = cellList.count() / width

        assert(
            height === Math.floor(height),
            `Recived non-integer height: ${height}`
        )

        this.height = height
    }

    public moveX(direction: Direction, layer: number) {
        const minimumLayerIndex = layer * this.width

        const newState = this.cellList.reduce((accumulated, current, index) => {
            if (
                index < minimumLayerIndex ||
                index >= (layer + 1) * this.width
            ) {
                return accumulated
            }

            const copyFrom =
                minimumLayerIndex +
                ((index - minimumLayerIndex - direction + this.width) %
                    this.width)

            return accumulated.set(index, this.cellList.get(copyFrom)!)
        }, this.cellList)

        return new GameState(newState, this.width)
    }

    public get cells() {
        return this.cellList.toArray()
    }

    [Symbol.iterator]() {
        return this.cellList[Symbol.iterator]()
    }
}
