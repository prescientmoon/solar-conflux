import { GameState, Tile } from "../gameState";
import { chunkSize } from "../map";
import { Vec2 } from "./types";

export const allTiles = function* (
  state: GameState
): Generator<[Tile | null, Vec2]> {
  for (let chunkX = 0; chunkX < state.map.chunkMap.length; chunkX++) {
    for (let chunkY = 0; chunkY < state.map.chunkMap[chunkX].length; chunkY++) {
      const chunk = state.map.chunkMap[chunkX][chunkY];

      if (chunk === null) continue;

      for (let tileX = 0; tileX < chunk.length; tileX++) {
        for (let tileY = 0; tileY < chunk[tileX].length; tileY++) {
          const tile = chunk[tileX][tileY];

          yield [
            tile,
            [chunkX * chunkSize + tileX, chunkY * chunkSize + tileY],
          ];
        }
      }
    }
  }
};
