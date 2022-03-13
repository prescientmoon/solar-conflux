import * as PIXI from "pixi.js";
import { LayerId, SimulationState, State } from "../State";
import { Vector2 } from "./Vector";

/** Return the pixi container for the nth layer */
export function getLayer(state: State, layer: LayerId) {
  const layerContainer = state.components.pixiObject.ref[state.camera].children[
    layer
  ] as PIXI.Container;

  return layerContainer;
}

/** Query the ecs for the position of an entity */
export function getPosition(state: SimulationState, eid: number): Vector2 {
  return state.components.transform[eid].position;
}

/** Query the ecs for the velocity of an entity */
export function getVelocity(state: SimulationState, eid: number): Vector2 {
  return state.components.velocity[eid];
}
