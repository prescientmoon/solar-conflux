import { GameState, loadAsset, Renderer, Tile } from "../gameState";
import { next, prev } from "../utils/direction";
import * as G from "../utils/iterate";
import { allTiles } from "../utils/traversals";
import { Direction } from "../utils/types";
import { renderTileWithDirection } from "./utils/renderTileWithDirection";

const textures = {
  straight: loadAsset("assets/belt_straight.svg"),
  bentLeft: loadAsset("assets/belt_bent_left.svg"),
  bentRight: loadAsset("assets/belt_bent_right.svg"),
};

const tiles = G.emptyComposer<GameState>()
  .then(G.map((s) => s.map.chunkMap))
  .then(G.biArray())
  .then(G.nullable())
  .then(G.biArray())
  .then(G.nullable()).unwrap;

export const beltRenderer: Renderer = {
  z: 0,
  render: (state) => {
    state.ctx.fillStyle = "green";

    for (const [tile, position] of allTiles(state)) {
      if (tile === null) continue;
      if (tile.machine.type !== "belt") continue;

      let texture = textures.straight;

      if (tile.machine.inputs.length === 1) {
        if (next(tile.machine.direction) === tile.machine.inputs[0])
          texture = textures.bentRight;
        if (prev(tile.machine.direction) === tile.machine.inputs[0])
          texture = textures.bentLeft;
      }

      renderTileWithDirection(
        state.ctx,
        tile.machine.direction,
        [
          position[0] * state.settings.tileSize,
          position[1] * state.settings.tileSize,
        ],
        state.settings.tileSize,
        () => {
          state.ctx.drawImage(
            texture,
            0,
            0,
            state.settings.tileSize,
            state.settings.tileSize
          );

          const xPos =
            (state.settings.tileSize - state.settings.itemOnBeltSize) / 2;

          for (const { item, position } of tile.machine.items) {
            state.ctx.drawImage(
              state.items[item].texture,
              xPos,
              (position * state.settings.tileSize) / 100,
              state.settings.itemOnBeltSize,
              state.settings.itemOnBeltSize
            );
          }
        }
      );
    }
  },
};
