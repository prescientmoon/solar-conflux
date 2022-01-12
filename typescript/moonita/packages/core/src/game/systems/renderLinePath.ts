import { Vector2 } from "../common/Transform";
import { LayerId, State } from "../State";

export type LinePath = Array<Vector2>;

export const renderLinePath = (
  state: State,
  layer: LayerId,
  path: LinePath
) => {
  if (path.length === 0) return;

  const context = state.contexts[layer];

  context.beginPath();
  context.moveTo(path[0].x, path[0].y);

  for (let i = 1; i < path.length; i++) {
    context.lineTo(path[i].x, path[i].y);
  }

  context.stroke();
};
