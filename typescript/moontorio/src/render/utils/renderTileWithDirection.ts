import { Image } from "../../gameState";
import { Direction, Pair, Vec2 } from "../../utils/types";

export const renderTileWithDirection = (
  ctx: CanvasRenderingContext2D,
  direction: Direction,
  tile: Image,
  position: Vec2,
  size: number
) => {
  if (direction === Direction.Up) {
    ctx.drawImage(tile, ...position, size, size);
  } else if (direction === Direction.Down) {
    ctx.save();
    ctx.translate(position[0] + size, position[1] + size);
    ctx.rotate(Math.PI);
    ctx.drawImage(tile, 0, 0, size, size);
    ctx.restore();
  } else if (direction === Direction.Right) {
    ctx.save();
    ctx.translate(position[0] + size, position[1]);
    ctx.rotate(Math.PI / 2);
    ctx.drawImage(tile, 0, 0, size, size);
    ctx.restore();
  } else if (direction === Direction.Left) {
    ctx.save();
    ctx.translate(position[0], position[1] + size);
    ctx.rotate(-Math.PI / 2);
    ctx.drawImage(tile, 0, 0, size, size);
    ctx.restore();
  }
};
