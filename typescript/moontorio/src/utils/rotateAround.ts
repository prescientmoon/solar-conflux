import { Vec2Like } from "@thi.ng/vectors";

export const rotateAround = (
  ctx: CanvasRenderingContext2D,
  position: Vec2Like,
  amount: number
) => {
  ctx.translate(position[0], position[1]);
  ctx.rotate(amount);
  ctx.translate(-position[0], -position[1]);
};
