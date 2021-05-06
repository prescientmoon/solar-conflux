import { GameState, Machine, Tile } from "../gameState";
import { chunkSize } from "../map";
import { Entity } from "../utils/entity";
import { Pair, Vec2 } from "../utils/types";

export const splitPosition = (position: Vec2): Pair<Vec2> => [
  [Math.floor(position[0] / chunkSize), Math.floor(position[1] / chunkSize)],
  [position[0] % chunkSize, position[1] % chunkSize],
];
export const tileAt = (state: GameState, position: Vec2): Tile | null =>
  state.map.chunkMap[Math.floor(position[0] / chunkSize)]?.[
    Math.floor(position[1] / chunkSize)
  ]?.[position[0] % chunkSize]?.[position[1] % chunkSize] ?? null;

export const machineAt = (world: GameState, position: Vec2): Entity | null => {
  const tile = tileAt(world, position);

  if (!tile) return null;

  return tile.machine;
};
