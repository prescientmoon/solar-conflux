import { Vector2 } from "../common/Vector";
import { LayerId, State } from "../State";

export type LinePath = Array<Vector2>;

export const renderLine = (
  context: CanvasRenderingContext2D,
  from: Vector2,
  to: Vector2
) => {
  context.beginPath();
  context.moveTo(from.x, from.y);
  context.lineTo(to.x, to.y);
  context.stroke();
};

export const renderLinePath = (
  context: CanvasRenderingContext2D,
  path: LinePath
) => {
  if (path.length === 0) return;

  context.beginPath();
  context.moveTo(path[0].x, path[0].y);

  for (let i = 1; i < path.length; i++) {
    context.lineTo(path[i].x, path[i].y);
  }

  context.stroke();
};
