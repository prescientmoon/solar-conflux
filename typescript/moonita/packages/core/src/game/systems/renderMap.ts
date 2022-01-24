import Quadtree from "@timohausmann/quadtree-js";
import { TextureId } from "../assets";
import { Flag } from "../common/Flags";
import { baseSize } from "../Map";
import { LayerId, State } from "../State";
import { renderCustomArrow } from "./debugArrows";
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

  for (let i = 0; i < state.map.teams.length; i++) {
    const team = state.map.teams[i];

    for (let p = 0; p < team.creaturePath.length; p++) {
      const from = p === 0 ? team.base : team.creaturePath[p - 1].position;
      const to = team.creaturePath[p].position;

      renderCustomArrow(state.contexts[LayerId.DebugLayer], from, to);
    }
  }
};

/** Renders the bounds of the world for debugging purpouses */
export function renderDebugBounds(state: State) {
  if (!state.flags[Flag.DebugShowBounds]) return;

  const context = state.contexts[LayerId.DebugLayer];

  context.strokeStyle = "black";
  context.lineWidth = 10;

  context.strokeRect(
    state.bounds[0].x,
    state.bounds[0].y,
    state.bounds[1].x - state.bounds[0].x,
    state.bounds[1].y - state.bounds[0].y
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
