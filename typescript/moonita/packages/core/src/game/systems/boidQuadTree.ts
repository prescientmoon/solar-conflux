import { State } from "../State";

export function insertBoidIntoQuadTree(state: State, id: number, team: number) {
  const tree = state.structures.boidQuadTrees[team];

  tree.insert(id);
}

export function updateBoidQuadTree(state: State, team: number) {
  const tree = state.structures.boidQuadTrees[team];

  tree.moveEntities();
  tree.cleanup();
}
