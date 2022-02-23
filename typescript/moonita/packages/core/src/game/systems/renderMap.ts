import { Flag } from "../common/Flags";
import { State } from "../State";
import { renderCustomArrow } from "./debugArrows";
import { renderLinePath } from "./renderLinePath";

export const renderDebugPaths = (state: State) => {
  if (!state.flags[Flag.DebugShowBasePaths]) return;

  const context = state.context;

  context.save();
  context.globalAlpha = 0.1;
  context.strokeStyle = "#000000";
  context.lineCap = "round";

  for (let i = 0; i < state.map.teams.length; i++) {
    const path = state.paths[state.map.teams[i].creaturePath];
    context.lineWidth = 2 * path.radius;

    renderLinePath(
      context,
      path.points.map((p) => p.position)
    );
  }

  context.restore();

  context.save();
  context.strokeStyle = "black";
  context.lineWidth = 2;

  for (let i = 0; i < state.map.teams.length; i++) {
    const path = state.paths[state.map.teams[i].creaturePath];

    for (let p = 1; p < path.points.length; p++) {
      const from = path.points[p - 1].position;
      const to = path.points[p].position;

      renderCustomArrow(context, from, to);
    }
  }
  context.restore();
};

/** Renders the bounds of the world for debugging purpouses */
export function renderDebugBounds(state: State) {
  if (!state.flags[Flag.DebugShowBounds]) return;

  const context = state.context;

  context.strokeStyle = "black";
  context.lineWidth = 10;

  context.strokeRect(
    state.bounds.position.x,
    state.bounds.position.y,
    state.bounds.size.x,
    state.bounds.size.y
  );
}

export function renderDebugQuadTrees(state: State) {
  if (!state.flags[Flag.DebugShowQuadTree]) return;

  for (let i = 0; i < state.map.teams.length; i++)
    state.structures.boidQuadTrees[i].render(state.context);
}
