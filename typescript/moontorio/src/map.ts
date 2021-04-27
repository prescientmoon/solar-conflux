import { Chunk, Tile } from "./gameState";
import { replicate } from "./utils/array";

export const chunkSize = 32;

export const createChunk = (): Chunk =>
  replicate(() => replicate(() => null, chunkSize), chunkSize);
