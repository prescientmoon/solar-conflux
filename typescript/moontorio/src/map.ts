import { GameState } from "./gameState";
import { creteChunk } from "./systems/serialize";
import { Vec2 } from "./utils/types";

export const addChunk = (world: GameState, position: Vec2) => {
  world.map.chunkMap.set(position, creteChunk(world, position));
};
