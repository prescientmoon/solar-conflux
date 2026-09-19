import { SimulationState } from "../State";
import * as V from "../common/Vector";

export function limitSpeeds(state: SimulationState) {
  state.queries.limitSpeeds._forEach((eid) => {
    const velocity = state.components.velocity[eid];

    V.limitMagnitudeMut(velocity, velocity, state.components.speedLimit[eid]);
  });
}
