import { State } from "../State";

export const moveEntities = (state: State) => {
  state.queries.kinematics._forEach((eid) => {
    state.components.transform.position.x[eid] +=
      state.components.velocity.x[eid];
    state.components.transform.position.y[eid] +=
      state.components.velocity.y[eid];
  });
};
