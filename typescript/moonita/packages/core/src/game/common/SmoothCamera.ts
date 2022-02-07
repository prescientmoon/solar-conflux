import { applyGlobalCameraObject } from "../systems/renderWithTransform";
import { Camera, identityCamera } from "./Camera";
import { Vector2 } from "./Vector";

export interface SmoothCamera {
  target: Camera;
  real: Camera;
  steps: number;
  updatesRemaining: number;
}

function interpolate(steps: number, start: number, end: number) {
  if (steps <= 0) throw new Error(`Invalid step count`);

  return start + (end - start) / steps;
}

function interpolate2dMut(
  steps: number,
  out: Vector2,
  start: Vector2,
  end: Vector2
): Vector2 {
  out.x = interpolate(steps, start.x, end.x);
  out.y = interpolate(steps, start.y, end.y);

  return out;
}

/** Commits changes made to the camera, telling the update function to update it. */
export function commit(camera: SmoothCamera) {
  camera.updatesRemaining = camera.steps;
}

/** Move the real camera towards the target */
export function update(camera: SmoothCamera) {
  if (camera.updatesRemaining <= 0) return;

  interpolate2dMut(
    camera.updatesRemaining,
    camera.real.scale,
    camera.real.scale,
    camera.target.scale
  );

  interpolate2dMut(
    camera.updatesRemaining,
    camera.real.position,
    camera.real.position,
    camera.target.position
  );

  camera.updatesRemaining--;
}

/** Constructs a smooth camera where both the real thing
 * and the target are at (0,0) with zoom 1 */
export function identity(steps: number): SmoothCamera {
  return {
    steps,
    updatesRemaining: 0,
    target: identityCamera(),
    real: identityCamera(),
  };
}
