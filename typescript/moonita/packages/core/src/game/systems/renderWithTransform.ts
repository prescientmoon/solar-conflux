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
  if (x || y) context.translate(x, y);
  if (scaleX !== 1 || scaleY !== 1) context.scale(scaleX, scaleY);
  if (rotation !== 0) context.rotate(rotation);
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
  for (const context of state.contexts) {
    if (camera.scale.x !== 1 || camera.scale.y !== 1)
      context.scale(camera.scale.x, camera.scale.y);
    if (camera.position.x || camera.position.y)
      context.translate(camera.position.x, camera.position.y);
  }
}
