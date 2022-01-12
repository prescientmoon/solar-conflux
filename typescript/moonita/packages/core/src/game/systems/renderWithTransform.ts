import { Transform } from "../common/Transform";
import { State } from "../State";

export function applyTransform(
  context: CanvasRenderingContext2D,
  x: number,
  y: number,
  rotation: number,
  scaleX: number,
  scaleY: number
) {
  context.translate(x, y);
  context.rotate(rotation);
  context.scale(scaleX, scaleY);
}

export function applyGlobalTransform(
  state: State,
  x: number,
  y: number,
  rotation: number,
  scaleX: number,
  scaleY: number
) {
  for (const context of state.contexts) {
    applyTransform(context, x, y, rotation, scaleX, scaleY);
  }
}

export function applyGlobalTransformObject(
  state: State,
  transform2d: Transform
) {
  applyGlobalTransform(
    state,
    transform2d.position.x,
    transform2d.position.y,
    transform2d.rotation,
    transform2d.scale.x,
    transform2d.scale.y
  );
}
