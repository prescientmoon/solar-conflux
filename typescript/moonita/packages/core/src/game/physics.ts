import { Vector2 } from "./common/Vector";
import { State } from "./State";

export function applyForce(state: State, entity: number, force: Vector2) {
  const mass = state.components.physicsObject.mass[entity];

  if (mass === undefined)
    throw new Error(`Entity ${entity} is not a physical object`);

  state.components.acceleration.x[entity] += force.x / mass;
  state.components.acceleration.y[entity] += force.y / mass;

  return entity;
}
