import { settings } from "../constants";
import { GameState, Tile } from "../gameState";
import { Vec2 } from "../utils/types";

export const renderSimpleTile = (
  state: GameState,
  tile: Tile,
  position: Vec2
) => {
  const texture = state.items[tile.machine.item].tileTexture;

  if (texture === undefined) return;

  state.ctx.drawImage(
    texture,
    position[0] * settings.tileSize,
    position[1] * settings.tileSize,
    settings.tileSize,
    settings.tileSize
  );
};
