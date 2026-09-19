import * as V from "../common/Vector";
import * as E from "wolf-ecs";
import { Flag } from "../common/Flags";
import { SimpleSystem } from "../State";

export const updateVelocities = SimpleSystem(
  (components) =>
    E.all<any>(
      components.transform,
      components.velocity,
      components.acceleration
    ),
  (state, eid) => {
    const acceleration = state.components.acceleration[eid];

    V.addMut(
      state.components.velocity[eid],
      state.components.velocity[eid],
      acceleration
    );

    // TODO: rethink this
    acceleration.x = 0;
    acceleration.y = 0;
  }
);

export const moveEntities = SimpleSystem(
  (components) => E.all<any>(components.transform, components.velocity),
  (state, eid) => {
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
  }
);

export const rotateEntities = SimpleSystem(
  (components) => E.all<any>(components.transform, components.angularVelocity),
  (state, eid) => {
    state.components.transform[eid].rotation +=
      state.components.angularVelocity[eid];
  }
);

// This *could* be done in a lazy way, but I myself am too lazy to implement that
export const updateOffsetOnlyChildrenPositions = SimpleSystem(
  (components) =>
    E.all<any>(components.transform, components.positionOnlyChild),
  (state, eid) => {
    const parent = state.components.positionOnlyChild.childOf[eid];
    const offset = state.components.positionOnlyChild.offset[eid];

    V.addMut(
      state.components.transform[eid].position,
      state.components.transform[parent].position,
      offset
    );
  }
);
