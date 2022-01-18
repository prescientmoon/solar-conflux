import { State } from "../State";
import { createBullet } from "./createEntity";

export const spawnBullets = (state: State) => {
  state.queries.bulletEmitters._forEach((eid) => {
    const createdAt = state.components.created.createdAt[eid];
    const frequency = state.components.bulletEmitter.frequency[eid];

    if ((state.tick - createdAt) % frequency === 0) {
      createBullet(state, eid, 10, 100);
    }
  });
};

// TODO: find a better way. This is wasteful
export const despawnBullets = (state: State) => {
  state.queries.mortal._forEach((eid) => {
    state.components.mortal.lifetime[eid]--;

    if (state.components.mortal.lifetime[eid] <= 0) {
      state.ecs.destroyEntity(eid);
    }
  });
};
