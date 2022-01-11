import { TextureId } from "../assets";
import { State } from "../State";
import { renderTexture } from "./renderTextures";

export const renderMap = (state: State) => {
  for (let i = 0; i < state.map.teams.length; i++) {
    const team = state.map.teams[i];

    renderTexture(
      state,
      TextureId.YellowBase,
      team.base.x,
      team.base.y,
      0,
      1,
      1,
      800,
      800
    );
  }
};
