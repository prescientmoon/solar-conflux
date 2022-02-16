import { State } from "../State";
import { createBullet } from "./createEntity";

export const spawnBullets = (state: State, eid: number) => {
  createBullet(state, eid, 10, 100);
};
