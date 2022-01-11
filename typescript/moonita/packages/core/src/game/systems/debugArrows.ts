import { Flag } from "../common/Flags";
import { Vector2, vectorDifference, vectorLength } from "../common/Transform";
import { State } from "../State";

const defaultTipSize: Vector2 = { x: 15, y: 5 };

/** Render an arrow of arbitrary length on the x-axis, with a custom sized tip */
export function renderArrow(
  state: State,
  length: number,
  tipSize: Vector2 = defaultTipSize
) {
  state.ctx.beginPath();
  state.ctx.moveTo(0, 0);
  state.ctx.lineTo(length, 0);
  state.ctx.stroke();
  state.ctx.lineTo(length + tipSize.x, 0);
  state.ctx.lineTo(length, tipSize.y);
  state.ctx.lineTo(length, -tipSize.y);
  state.ctx.lineTo(length + tipSize.x, 0);
  state.ctx.fill();
}

export function renderCustomArrow(
  state: State,
  from: Vector2,
  to: Vector2,
  tipSize: Vector2 = defaultTipSize
) {
  state.ctx.save();

  state.ctx.translate(from.x, from.y);

  const delta = vectorDifference(to, from);
  const length = vectorLength(delta);
  const angle = Math.atan2(delta.x, delta.y);

  state.ctx.rotate(angle);

  renderArrow(state, length, tipSize);

  state.ctx.restore();
}

export function renderPerpendicularArrows(
  state: State,
  length: number,
  tipSize: Vector2 = defaultTipSize
) {
  state.ctx.save();
  state.ctx.lineWidth = 6;
  state.ctx.lineCap = "round";

  state.ctx.strokeStyle = "blue";
  state.ctx.fillStyle = "blue";
  renderArrow(state, length, tipSize);

  state.ctx.strokeStyle = "red";
  state.ctx.fillStyle = "red";
  state.ctx.rotate(Math.PI / 2);
  renderArrow(state, length, tipSize);

  state.ctx.restore();
}

export function renderDebugArrows(state: State) {
  if (state.flags[Flag.DebugShowOriginArrow]) {
    renderPerpendicularArrows(state, 50);
  }
}
