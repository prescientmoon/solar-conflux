import { getPosition } from "../common/Entity";
import { State } from "../State";

export function insertBoidIntoQuadTree(state: State, id: number) {
  const tree = state.structures.boidQuadTree;

  tree.insert(id);
}

export function updateBoidQuadTree(state: State) {
  const tree = state.structures.boidQuadTree;

  tree.moveEntities();
  tree.cleanup();

  // state.queries.boid.forEach((eid) => {
  //   insertBoidIntoQuadTree(state, eid);
  // });
}
