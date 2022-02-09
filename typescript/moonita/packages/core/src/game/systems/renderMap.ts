import { TextureId } from "../assets";
import { Flag } from "../common/Flags";
import { baseSize } from "../Map";
import { LayerId, State } from "../State";
import { renderCustomArrow } from "./debugArrows";
import { renderLine, renderLinePath } from "./renderLinePath";
import { renderTexture } from "./renderTextures";

export const renderMap = (state: State) => {
  for (let i = 0; i < state.map.teams.length; i++) {
    const team = state.map.teams[i];

    renderTexture(
      state,
      LayerId.BuildingLayer,
      TextureId.YellowBase,
      team.base.x,
      team.base.y,
      team.base.rotation,
      1,
      1,
      baseSize,
      baseSize
    );
  }
};

export const renderDebugPaths = (state: State) => {
  if (!state.flags[Flag.DebugShowBasePaths]) return;

  const context = state.contexts[LayerId.DebugLayer];

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

  const context = state.contexts[LayerId.DebugLayer];

  context.strokeStyle = "black";
  context.lineWidth = 10;

  context.strokeRect(
    state.bounds.position.x,
    state.bounds.position.y,
    state.bounds.size.x,
    state.bounds.size.y
  );

  const drawQuadtree = function (node: any) {
    var bounds = node.bounds;

    //no subnodes? draw the current node
    if (node.nodes.length === 0) {
      if (node.check) {
        context.strokeStyle = "rgba(255,255,255,1)";
      } else {
        context.strokeStyle = "rgba(255,0,0,0.5)";
      }
      context.strokeRect(bounds.x, bounds.y, bounds.width, bounds.height);

      //has subnodes? drawQuadtree them!
    } else {
      for (var i = 0; i < node.nodes.length; i = i + 1) {
        drawQuadtree(node.nodes[i]);
      }
    }
  };

  drawQuadtree(state.structures.boidQuadTree);
}

export function renderDebugQuadTrees(state: State) {
  if (!state.flags[Flag.DebugShowQuadTree]) return;

  state.structures.boidQuadTree.render(state.contexts[LayerId.DebugLayer]);
}
