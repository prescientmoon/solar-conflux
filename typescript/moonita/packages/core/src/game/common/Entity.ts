import { TypedArray } from "wolf-ecs";
import { State } from "../State";
import { Vector2 } from "./Vector";

/** Query the ecs for a vec */
export function getEntityVec(
  target: { x: TypedArray; y: TypedArray },
  eid: number
): Vector2 {
  return {
    x: target.x[eid] as number,
    y: target.y[eid] as number,
  };
}

/** Update a vec in the ecs */
export function setEntityVec(
  target: { x: TypedArray; y: TypedArray },
  eid: number,
  value: Vector2
) {
  target.x[eid] = value.x;
  target.y[eid] = value.y;
}

/** Query the ecs for the position of an entity */
export function getPosition(state: State, eid: number): Vector2 {
  return {
    x: state.components.transform.position.x[eid],

    y: state.components.transform.position.y[eid],
  };
}

/** Query the ecs for the velocity of an entity */
export function getVelocity(state: State, eid: number): Vector2 {
  return {
    x: state.components.velocity.x[eid],

    y: state.components.velocity.y[eid],
  };
}
