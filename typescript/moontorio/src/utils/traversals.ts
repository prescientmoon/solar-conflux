import { GameState, Tile } from "../gameState";

export const allTiles = function* (state: GameState): Generator<Tile | null> {
  for (const chunk of state.map.chunkMap) {
    for (const tile of chunk) {
      yield tile;
    }
  }
};
