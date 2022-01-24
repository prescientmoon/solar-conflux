import { getPosition } from "../common/Entity";
import { State } from "../State";
import * as AABB from "../common/AABB";
import { maxBoidRadius } from "../common/Settings";

export function boidQuadTreeNode(
  state: State,
  eid: number,
  radius = maxBoidRadius
) {
  const position = getPosition(state, eid);

  return AABB.toRect(AABB.fromSquareCenter(position, radius));
}

export function insertBoidIntoQuadTree(state: State, eid: number) {
  const tree = state.structures.boidQuadTree;

  const node = {
    eid,
    ...boidQuadTreeNode(state, eid),
  };

  tree.insert(node);
}

export function updateBoidQuadTree(state: State) {
  const tree = state.structures.boidQuadTree;

  tree.clear();

  state.queries.boid.forEach((eid) => {
    insertBoidIntoQuadTree(state, eid);
  });
}
