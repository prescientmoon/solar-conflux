import { GameState, loadAsset } from "./gameState";

export interface Player {
  position: [number, number];
}

const playerTexture = loadAsset("assets/player.png");

export const renderPlayer = (state: GameState) => {
  const player = state.player;

  state.ctx.drawImage(playerTexture, player.position[0], player.position[1]);
};

export const updatePlayer = (state: GameState) => {
  if (state.keyboard.pressed.has("w")) {
    state.player.position[0] += 1;
  }

  if (state.keyboard.pressed.has("s")) {
    state.player.position[0] -= 1;
  }
};
