import { settings } from "../constants";
import { GameState, Tile } from "../gameState";
import { Vec2 } from "../utils/types";

export const renderSimpleTile = (
  state: GameState,
  tile: Tile,
  position: Vec2
) => {
  state.ctx.drawImage(
    state.items[tile.machine.item].texture,
    position[0],
    position[1],
    settings.tileSize,
    settings.tileSize
  );
};
