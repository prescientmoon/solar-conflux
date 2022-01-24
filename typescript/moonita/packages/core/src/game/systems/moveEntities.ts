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
      if (state.components.transform.position.x[eid] < state.bounds[0].x)
        state.components.transform.position.x[eid] = state.bounds[1].x;
      else if (state.components.transform.position.x[eid] > state.bounds[1].x)
        state.components.transform.position.x[eid] = state.bounds[0].x;
      if (state.components.transform.position.y[eid] < state.bounds[0].y)
        state.components.transform.position.y[eid] = state.bounds[1].y;
      else if (state.components.transform.position.y[eid] > state.bounds[1].y)
        state.components.transform.position.y[eid] = state.bounds[0].y;
    }
  });
}

export function rotateEntities(state: State) {
  state.queries.rotating._forEach((eid) => {
    state.components.transform.rotation[eid] +=
      state.components.angularVelocity[eid];
  });
}
