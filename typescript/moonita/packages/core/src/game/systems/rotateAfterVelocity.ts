import { State } from "../State";

/** Rotates all entities (which opted into this)
 * towards the direction they are moving in.
 */
export function rotateAfterVelocity(state: State) {
  state.queries.rotateAfterVelocity._forEach((eid) => {
    const angle = Math.atan2(
      state.components.velocity.y[eid],
      state.components.velocity.x[eid]
    );

    state.components.transform.rotation[eid] = angle;
  });
}
