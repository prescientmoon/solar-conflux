import { Flag } from "../common/Flags";
import { Vector2, vectorDifference, vectorLength } from "../common/Transform";
import { LayerId, State } from "../State";

const defaultTipSize: Vector2 = { x: 15, y: 5 };

/** Render an arrow of arbitrary length on the x-axis, with a custom sized tip */
export function renderArrow(
  context: CanvasRenderingContext2D,
  length: number,
  tipSize: Vector2 = defaultTipSize
) {
  context.beginPath();
  context.moveTo(0, 0);
  context.lineTo(length, 0);
  context.stroke();
  context.lineTo(length + tipSize.x, 0);
  context.lineTo(length, tipSize.y);
  context.lineTo(length, -tipSize.y);
  context.lineTo(length + tipSize.x, 0);
  context.fill();
}

export function renderCustomArrow(
  context: CanvasRenderingContext2D,
  from: Vector2,
  to: Vector2,
  tipSize: Vector2 = defaultTipSize
) {
  context.save();

  context.translate(from.x, from.y);

  const delta = vectorDifference(to, from);
  const length = vectorLength(delta);
  const angle = Math.atan2(delta.x, delta.y);

  context.rotate(angle);

  renderArrow(context, length, tipSize);

  context.restore();
}

export function renderPerpendicularArrows(
  context: CanvasRenderingContext2D,
  length: number,
  tipSize: Vector2 = defaultTipSize
) {
  context.save();
  context.lineWidth = 6;
  context.lineCap = "round";

  context.strokeStyle = "blue";
  context.fillStyle = "blue";
  renderArrow(context, length, tipSize);

  context.strokeStyle = "red";
  context.fillStyle = "red";
  context.rotate(Math.PI / 2);
  renderArrow(context, length, tipSize);

  context.restore();
}

export function renderDebugArrows(state: State) {
  if (!state.flags[Flag.DebugShowOriginArrow]) return;

  renderPerpendicularArrows(state.contexts[LayerId.DebugLayer], 50);
}
