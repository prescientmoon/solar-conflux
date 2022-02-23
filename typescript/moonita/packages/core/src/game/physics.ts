import { Vector2 } from "./common/Vector";
import { SimulationState } from "./State";

export function applyForce(
  state: SimulationState,
  entity: number,
  force: Vector2
): number {
  const mass = state.components.physicsObject.mass[entity];

  if (mass === undefined)
    throw new Error(`Entity ${entity} is not a physical object`);

  const acceleration = state.components.acceleration[entity];
  acceleration.x += force.x / mass;
  acceleration.y += force.y / mass;

  return entity;
}
