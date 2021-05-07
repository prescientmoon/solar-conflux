import { GameState, Machine, Tile } from "../gameState";
import { chunkSize } from "../map";
import { Entity } from "../utils/entity";
import { Pair, Vec2 } from "../utils/types";

export const splitPosition = (position: Vec2): [Vec2, Vec2, Vec2] => [
  [
    Math.abs(Math.floor(position[0] / chunkSize)),
    Math.abs(Math.floor(position[1] / chunkSize)),
  ],
  [Math.abs(position[0] % chunkSize), Math.abs(position[1] % chunkSize)],
  [position[0] >= 0 ? 0 : 1, position[1] >= 0 ? 0 : 1],
];
export const tileAt = (state: GameState, position: Vec2): Tile | null =>
  state.map.chunkMap[position[0] >= 0 ? 0 : 1][position[1] >= 0 ? 0 : 1][
    Math.abs(Math.floor(position[0] / chunkSize))
  ][Math.abs(Math.floor(position[1] / chunkSize))]?.[
    Math.abs(position[0] % chunkSize)
  ][Math.abs(position[1] % chunkSize)] ?? null;

/**
 * Sets the tile at an absolute position. Assume the chunk exists.
 */
export const setTileAt = (
  state: GameState,
  position: Vec2,
  tile: Tile | null
) => {
  state.map.chunkMap[position[0] >= 0 ? 0 : 1][position[1] >= 0 ? 0 : 1][
    Math.abs(Math.floor(position[0] / chunkSize))
  ][Math.abs(Math.floor(position[1] / chunkSize))]![
    Math.abs(position[0] % chunkSize)
  ][Math.abs(position[1] % chunkSize)] = tile;
};

export const machineAt = (state: GameState, position: Vec2): Entity | null => {
  const tile = tileAt(state, position);
  // console.log(tileAt(world, [-1, -1]));
  // console.log(
  //     state.map.chunkMap[position[0] >= 0 ? 0 : 1][position[1] >= 0 ? 0 : 1]
  // );

  if (!tile) return null;

  return tile.machine;
};
