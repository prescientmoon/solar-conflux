import type {
  Chunk,
  DirectionChunkMatrixes,
  GameState,
  Machine,
} from "../gameState";
import { chunkSize } from "../map";
import {
  decodeFixedMatrix,
  decodeMatrix,
  decodeNumber,
  decodeOptionalField,
  decodePair,
  decodeRecord,
  Json,
  oneOf,
} from "../utils/json";
import { Vec2 } from "../utils/types";
import { ConveyorBelt } from "./belts";
import { Chest } from "./chest";
import { Junction } from "./junction";
import { Loader, Unloader } from "./loaders";
import { Router } from "./router";
import { machineAt } from "./world";

export const encodeWorld = (world: GameState): Json => {
  const map = world.map.chunkMap.map((i) =>
    i.map((i) =>
      i.map((i) =>
        i.map((i) =>
          i?.map((i) =>
            i.map((i) => {
              if (i === null) return null;
              if (i.subTile[0] || i.subTile[1]) return { subTile: i.subTile };

              return {
                subTile: i.subTile,
                machine: i.machine.encode(),
              };
            })
          )
        )
      )
    )
  );

  return {
    tick: world.tick,
    player: (world.player as any) as Json,
    camera: world.camera,
    map,
  };
};

export const decodeWorld = (json: Json, world: GameState) => {
  const { map, camera, player, tick } = decodeRecord({
    tick: decodeNumber,
    camera: decodeRecord({
      scale: decodeNumber,
      translation: decodePair(decodeNumber),
    }),
    player: decodeRecord({
      rotation: decodeNumber,
      speedMultiplier: decodeNumber,
      position: decodePair(decodeNumber),
    }),
    map: (a) => a,
  })(json);

  world.camera = camera;
  world.player = player;
  world.tick = tick;

  const chunkMapJson = decodeFixedMatrix(2, (a) => a)(map);
  const chunks: DirectionChunkMatrixes[][] = [
    [[], []],
    [[], []],
  ];

  world.map = { chunkMap: chunks };

  for (const chunkDirectionX of [0, 1]) {
    for (const chunkDirectionY of [0, 1]) {
      const chunkGroup = decodeMatrix((a) => a)(
        chunkMapJson[chunkDirectionX][chunkDirectionY]
      );

      for (let chunkX = 0; chunkX < chunkGroup.length; chunkX++) {
        if (chunks[chunkDirectionX][chunkDirectionY][chunkX] === undefined)
          chunks[chunkDirectionX][chunkDirectionY][chunkX] = [];

        for (let chunkY = 0; chunkY < chunkGroup[chunkX].length; chunkY++) {
          if (chunkGroup[chunkX][chunkY] === null) {
            chunks[chunkDirectionX][chunkDirectionY][chunkX][chunkY] = null;
            continue;
          }

          const chunkJson = decodeFixedMatrix(
            chunkSize,
            (a) => a
          )(chunkGroup[chunkX][chunkY]);

          const chunk: Chunk = [];
          chunks[chunkDirectionX][chunkDirectionY][chunkX][chunkY] = chunk;

          for (let tileX = 0; tileX < chunkSize; tileX++) {
            for (let tileY = 0; tileY < chunkSize; tileY++) {
              const tileJson = chunkJson[tileX][tileY];

              if (tileJson === null) {
                chunk[tileX][tileY] = null;
                continue;
              }

              const { subTile } = decodeRecord({
                subTile: decodePair(decodeNumber),
              })(tileJson);

              const position: Vec2 = [
                (chunkDirectionX === 0 ? -1 : 1) * (chunkX * chunkSize + tileX),
                (chunkDirectionY === 0 ? -1 : 1) * (chunkY * chunkSize + tileY),
              ];

              const machine = decodeOptionalField(
                `machine`,
                oneOf<Machine>(
                  (json) => ConveyorBelt.decode(json, world),
                  (json) => Router.decode(json, world, position),
                  (json) => Junction.decode(json, world, position),
                  (json) => Chest.decode(json, world, position),
                  (json) => Loader.decode(json, world, position),
                  (json) => Unloader.decode(json, world, position)
                )
              )(tileJson);

              chunk[tileX][tileY] = {
                subTile,
                machine: machine ?? machineAt(world, position)!,
              };
            }
          }
        }
      }
    }
  }
};

// @ts-ignore
window.save = encodeWorld;
// @ts-ignore
window.decode = decodeWorld;
