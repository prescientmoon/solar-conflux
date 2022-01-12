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
