import { SimulationState } from "../State";

export function insertBoidIntoQuadTree(
  state: SimulationState,
  id: number,
  team: number
) {
  const tree = state.structures.boidQuadTrees[team];

  tree.insert(id);
}

export function updateBoidQuadTree(state: SimulationState, team: number) {
  const tree = state.structures.boidQuadTrees[team];

  tree.moveEntities();
  tree.cleanup();
}
