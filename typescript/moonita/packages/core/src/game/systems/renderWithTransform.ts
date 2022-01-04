import { Transform } from "../common/Transform";
import { State } from "../State";

export function applyTransform(
  state: State,
  x: number,
  y: number,
  rotation: number,
  scaleX: number,
  scaleY: number
) {
  state.ctx.translate(x, y);
  state.ctx.rotate(rotation);
  state.ctx.scale(scaleX, scaleY);
}

export function applyTransformObject(state: State, transform2d: Transform) {
  applyTransform(
    state,
    transform2d.position.x,
    transform2d.position.y,
    transform2d.rotation,
    transform2d.scale.x,
    transform2d.scale.y
  );
}
