import { State } from "../State";

export function moveEntities(state: State) {
  state.queries.kinematics._forEach((eid) => {
    state.components.transform.position.x[eid] +=
      state.components.velocity.x[eid];
    state.components.transform.position.y[eid] +=
      state.components.velocity.y[eid];
  });
}

export function rotateEntities(state: State) {
  state.queries.rotating._forEach((eid) => {
    state.components.transform.rotation[eid] +=
      state.components.angularVelocity[eid];
  });
}
