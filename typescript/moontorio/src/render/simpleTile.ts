import { settings } from "../constants";
import { GameState, Machine, Tile } from "../gameState";
import { Vec2 } from "../utils/types";

export const renderSimpleTile = (self: Machine, item: string) => {
  const texture = self.world.items[item].tileTexture;

  if (texture === undefined) return;

  self.world.ctx.drawImage(
    texture,
    self.position[0] * settings.tileSize,
    self.position[1] * settings.tileSize,
    self.size[0] * settings.tileSize,
    self.size[1] * settings.tileSize
  );
};
