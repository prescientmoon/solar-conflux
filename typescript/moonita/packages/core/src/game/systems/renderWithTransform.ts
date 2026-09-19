import { Camera } from "../common/Camera";
import { Transform } from "../common/Transform";

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

export function applyGlobalTransformObject(
  context: CanvasRenderingContext2D,
  transform2d: Transform
) {
  applyTransform(
    context,
    transform2d.position.x,
    transform2d.position.y,
    transform2d.rotation,
    transform2d.scale.x,
    transform2d.scale.y
  );
}

export function applyCameraObject(
  context: CanvasRenderingContext2D,
  camera: Camera
) {
  if (camera.position.x || camera.position.y)
    context.translate(camera.position.x, camera.position.y);
  if (camera.scale.x !== 1 || camera.scale.y !== 1)
    context.scale(camera.scale.x, camera.scale.y);
}
