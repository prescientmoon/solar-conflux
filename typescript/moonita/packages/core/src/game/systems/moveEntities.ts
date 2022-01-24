import { Flag } from "../common/Flags";
import { State } from "../State";
import { setAcceleration } from "./createEntity";

export function updateVelocities(state: State) {
  state.queries.kinematics._forEach((eid) => {
    state.components.velocity.x[eid] += state.components.acceleration.x[eid];
    state.components.velocity.y[eid] += state.components.acceleration.y[eid];

    // TODO: rethink this
    setAcceleration(state, eid, 0, 0);
  });
}

export function moveEntities(state: State) {
  state.queries.kinematics._forEach((eid) => {
    state.components.transform.position.x[eid] +=
      state.components.velocity.x[eid];
    state.components.transform.position.y[eid] +=
      state.components.velocity.y[eid];

    if (state.flags[Flag.DebugWrapping]) {
      if (state.components.transform.position.x[eid] < state.bounds.position.x)
        state.components.transform.position.x[eid] =
          state.bounds.position.x + state.bounds.size.x;
      else if (
        state.components.transform.position.x[eid] >
        state.bounds.position.x + state.bounds.size.x
      )
        state.components.transform.position.x[eid] = state.bounds.position.x;
      if (state.components.transform.position.y[eid] < state.bounds.position.y)
        state.components.transform.position.y[eid] =
          state.bounds.position.y + state.bounds.size.y;
      else if (
        state.components.transform.position.y[eid] >
        state.bounds.position.y + state.bounds.size.y
      )
        state.components.transform.position.y[eid] = state.bounds.position.y;
    }
  });
}

export function rotateEntities(state: State) {
  state.queries.rotating._forEach((eid) => {
    state.components.transform.rotation[eid] +=
      state.components.angularVelocity[eid];
  });
}
