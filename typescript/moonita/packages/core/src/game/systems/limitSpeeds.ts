import { State } from "../State";
import * as V from "../common/Vector";
import { setVelocity } from "./createEntity";

export function limitSpeeds(state: State) {
  state.queries.limitSpeeds._forEach((eid) => {
    const velocity = {
      x: state.components.velocity.x[eid],
      y: state.components.velocity.y[eid],
    };

    V.limitMagnitudeMut(velocity, velocity, state.components.speedLimit[eid]);

    setVelocity(state, eid, velocity.x, velocity.y);
  });
}
