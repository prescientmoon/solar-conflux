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
      const l = 300;
      if (state.components.transform.position.x[eid] < -l)
        state.components.transform.position.x[eid] = l;
      else if (state.components.transform.position.x[eid] > l)
        state.components.transform.position.x[eid] = -l;
      if (state.components.transform.position.y[eid] < -l)
        state.components.transform.position.y[eid] = l;
      else if (state.components.transform.position.y[eid] > l)
        state.components.transform.position.y[eid] = -l;
    }
  });
}

export function rotateEntities(state: State) {
  state.queries.rotating._forEach((eid) => {
    state.components.transform.rotation[eid] +=
      state.components.angularVelocity[eid];
  });
}
