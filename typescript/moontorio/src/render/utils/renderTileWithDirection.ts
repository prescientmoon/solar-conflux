import { Image } from "../../gameState";
import { Direction, Fn, Lazy, Pair, Vec2 } from "../../utils/types";

export const renderTileWithDirection = (
  ctx: CanvasRenderingContext2D,
  direction: Direction,
  position: Vec2,
  size: number,
  draw: Fn<number, void>
) => {
  ctx.save();
  if (direction === Direction.Down) {
    ctx.translate(...position);
    draw(0);
  } else if (direction === Direction.Up) {
    ctx.translate(position[0] + size, position[1] + size);
    ctx.rotate(Math.PI);
    draw(Math.PI);
  } else if (direction === Direction.Left) {
    ctx.translate(position[0] + size, position[1]);
    ctx.rotate(Math.PI / 2);
    draw(Math.PI / 2);
  } else if (direction === Direction.Right) {
    ctx.translate(position[0], position[1] + size);
    ctx.rotate(-Math.PI / 2);
    draw(-Math.PI / 2);
  }
  ctx.restore();
};
