import { SimulationState } from "../State";
import { Vector2 } from "./Vector";

/** Query the ecs for the position of an entity */
export function getPosition(state: SimulationState, eid: number): Vector2 {
  return state.components.transform[eid].position;
}

/** Query the ecs for the velocity of an entity */
export function getVelocity(state: SimulationState, eid: number): Vector2 {
  return state.components.velocity[eid];
}
