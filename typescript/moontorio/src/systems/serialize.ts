import { sub2 } from "@thi.ng/vectors";
import { settings } from "../constants";
import type { Chunk, GameState, Machine, Tile } from "../gameState";
import {
  decodeNumber,
  decodeOptionalField,
  decodePair,
  decodeRecord,
  Json,
  oneOf,
} from "../utils/json";
import { FiniteMatrix } from "../utils/matrix";
import { Lazy, Nullable, Vec2 } from "../utils/types";
import { ConveyorBelt } from "./belts";
import { Chest } from "./chest";
import { Junction } from "./junction";
import { Loader, Unloader } from "./loaders";
import { Router } from "./router";
import { absoluteToSplit, machineAt } from "./world";

export const encodeWorld = (world: GameState): Json => {
  const map = world.map.chunkMap.encode();

  return {
    tick: world.tick,
    player: (world.player as any) as Json,
    camera: world.camera,
    map,
  };
};

const encodeTile = (tile: Nullable<Tile>): Json => {
  if (tile === null) return null;

  if (tile.subTile[0] || tile.subTile[1]) return { subTile: tile.subTile };

  return {
    subTile: tile.subTile,
    machine: tile.machine.encode(),
  };
};

export const decodeTile = (
  tileJson: Json,
  world: GameState,
  tilePosition: Vec2,
  chunkPosition: Vec2
) => {
  if (tileJson === null) return null;

  const { subTile } = decodeRecord({
    subTile: decodePair(decodeNumber),
  })(tileJson);

  const position = absoluteToSplit.undo({
    chunk: chunkPosition,
    tile: tilePosition,
  });

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

  if (position[0] === 1 && position[1] === 0) {
    console.log({ position, tilePosition, chunkPosition, tileJson, machine });
    console.log(world.map.chunkMap.get([0, 0])?.elements);
  }

  return {
    subTile,
    machine:
      machine ?? machineAt(world, sub2(null, position, subTile) as Vec2)!,
  };
};

export const creteChunk = (world: GameState, chunkPosition: Vec2) => {
  return new FiniteMatrix(
    settings.chunkSize,
    settings.chunkSize,
    encodeTile,
    (element, tilePosition) =>
      decodeTile(element, world, tilePosition, chunkPosition)
  );
};

export const createChunkmapElement = (lworld: Lazy<GameState>) => (
  chunkPosition: Vec2
): Chunk => {
  return creteChunk(lworld(), chunkPosition);
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
  world.map.chunkMap.decode(map);
};
