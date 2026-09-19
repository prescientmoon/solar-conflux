import { TAU } from "../../math";

/** Renders a circle. Does not fill/stroke for you */
export function renderCircle(
  ctx: CanvasRenderingContext2D,
  x: number,
  y: number,
  radius: number
) {
  ctx.beginPath();
  ctx.arc(x, y, radius, 0, TAU);
}
