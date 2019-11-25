import { rangeArray } from './rangeArray'
import { GameState } from '../classes/GameState'
import { List } from 'immutable'

export const createGame = (width: number, height: number) =>
    new GameState(List(rangeArray(0, width * height)), width)
