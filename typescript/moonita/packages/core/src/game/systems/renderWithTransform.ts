import { Camera } from "../common/Camera";
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
  context.scale(scaleX, scaleY);
  context.translate(x, y);
  context.rotate(rotation);
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

export function applyGlobalCameraObject(state: State, camera: Camera) {
  applyGlobalTransform(
    state,
    camera.position.x,
    camera.position.y,
    0,
    camera.scale.x,
    camera.scale.y
  );
}
