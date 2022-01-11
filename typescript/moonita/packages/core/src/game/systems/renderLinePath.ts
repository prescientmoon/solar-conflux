import { Vector2 } from "../common/Transform";
import { State } from "../State";

export type LinePath = Array<Vector2>;

export const renderLinePath = (state: State, path: LinePath) => {
  if (path.length === 0) return;

  state.ctx.beginPath();
  state.ctx.moveTo(path[0].x, path[0].y);

  for (let i = 1; i < path.length; i++) {
    state.ctx.lineTo(path[i].x, path[i].y);
  }

  state.ctx.stroke();
};
