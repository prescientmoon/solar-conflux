import { getPosition } from "../common/Entity";
import { State } from "../State";
import * as AABB from "../common/AABB";
import { maxBoidRadius } from "../common/Settings";

export function insertBoidIntoQuadTree(state: State, id: number) {
  const tree = state.structures.boidQuadTree;

  const position = getPosition(state, id);

  tree.insert({ position, id });
}

export function updateBoidQuadTree(state: State) {
  const tree = state.structures.boidQuadTree;

  tree.moveEntities(state.components.transform.position);
  tree.cleanup();

  // state.queries.boid.forEach((eid) => {
  //   insertBoidIntoQuadTree(state, eid);
  // });
}
