import { Image } from "../../gameState";
import { Direction, Lazy, Pair, Vec2 } from "../../utils/types";

export const renderTileWithDirection = (
  ctx: CanvasRenderingContext2D,
  direction: Direction,
  position: Vec2,
  size: number,
  draw: Lazy<void>
) => {
  ctx.save();
  if (direction === Direction.Down) {
    ctx.translate(...position);
    draw();
  } else if (direction === Direction.Up) {
    ctx.translate(position[0] + size, position[1] + size);
    ctx.rotate(Math.PI);
    draw();
  } else if (direction === Direction.Left) {
    ctx.translate(position[0] + size, position[1]);
    ctx.rotate(Math.PI / 2);
    draw();
  } else if (direction === Direction.Right) {
    ctx.translate(position[0], position[1] + size);
    ctx.rotate(-Math.PI / 2);
    draw();
  }
  ctx.restore();
};
