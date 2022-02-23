import { SimulationState } from "../State";

/** Rotates all entities (which opted into this)
 * towards the direction they are moving in.
 */
export function rotateAfterVelocity(state: SimulationState) {
  state.queries.rotateAfterVelocity._forEach((eid) => {
    const angle = Math.atan2(
      state.components.velocity[eid].y,
      state.components.velocity[eid].x
    );

    state.components.transform[eid].rotation = angle;
  });
}
