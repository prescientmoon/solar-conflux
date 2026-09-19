import { SimulationState } from "../State";
import { createBullet } from "./createEntity";

export const spawnBullets = (state: SimulationState, eid: number) => {
  createBullet(state, eid, 10, 100);
};
