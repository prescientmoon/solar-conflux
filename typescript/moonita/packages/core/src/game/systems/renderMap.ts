import { TextureId } from "../assets";
import { Flag } from "../common/Flags";
import { baseSize } from "../Map";
import { State } from "../State";
import { renderArrow, renderCustomArrow } from "./debugArrows";
import { renderLinePath } from "./renderLinePath";
import { renderTexture } from "./renderTextures";

export const renderMap = (state: State) => {
  for (let i = 0; i < state.map.teams.length; i++) {
    const team = state.map.teams[i];

    renderTexture(
      state,
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

      renderCustomArrow(state, from, to);
    }
  }
};
