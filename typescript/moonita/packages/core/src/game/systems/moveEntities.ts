import { Flag } from "../common/Flags";
import { SimulationState } from "../State";
import * as V from "../common/Vector";

export function updateVelocities(state: SimulationState) {
  state.queries.kinematics._forEach((eid) => {
    const acceleration = state.components.acceleration[eid];

    V.addMut(
      state.components.velocity[eid],
      state.components.velocity[eid],
      acceleration
    );

    // TODO: rethink this
    acceleration.x = 0;
    acceleration.y = 0;
  });
}

export function moveEntities(state: SimulationState) {
  state.queries.kinematics._forEach((eid) => {
    const transform = state.components.transform[eid];

    V.addMut(
      transform.position,
      transform.position,
      state.components.velocity[eid]
    );

    if (state.flags[Flag.DebugWrapping]) {
      if (transform.position.x < state.bounds.position.x)
        transform.position.x = state.bounds.position.x + state.bounds.size.x;
      else if (
        transform.position.x >
        state.bounds.position.x + state.bounds.size.x
      )
        transform.position.x = state.bounds.position.x;
      if (transform.position.y < state.bounds.position.y)
        transform.position.y = state.bounds.position.y + state.bounds.size.y;
      else if (
        transform.position.y >
        state.bounds.position.y + state.bounds.size.y
      )
        transform.position.y = state.bounds.position.y;
    }
  });
}

export function rotateEntities(state: SimulationState) {
  state.queries.rotating._forEach((eid) => {
    state.components.transform[eid].rotation +=
      state.components.angularVelocity[eid];
  });
}
