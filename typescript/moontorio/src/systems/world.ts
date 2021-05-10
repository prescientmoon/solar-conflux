import { settings } from "../constants";
import type {
  GameState,
  Machine,
  Tile,
  ItemOptions,
  Item,
  TimedItem,
  ItemConfig,
} from "../gameState";
import { pressedKeys } from "../keyboard";
import { EventEmitter } from "../utils/events";
import {
  decodeNumber,
  Decoder,
  decodeRecord,
  decodeString,
} from "../utils/json";
import { InfiniteMatrix, Isomorphism } from "../utils/matrix";
import type { Vec2 } from "../utils/types";
import { createChunkmapElement } from "./serialize";

const absMod = (a: number, mod: number) => {
  if (a < 0) return mod + (a % mod);

  return a % mod;
};

export const absoluteToSplit: Isomorphism<Vec2, { chunk: Vec2; tile: Vec2 }> = {
  do(input: Vec2) {
    return {
      chunk: [
        Math.floor(input[0] / settings.chunkSize),
        Math.floor(input[1] / settings.chunkSize),
      ],
      tile: [
        absMod(input[0], settings.chunkSize),
        absMod(input[1], settings.chunkSize),
      ],
    };
  },
  undo({ chunk, tile }) {
    return [
      tile[0] + chunk[0] * settings.chunkSize,
      tile[1] + chunk[1] * settings.chunkSize,
    ];
  },
};

export const tileAt = (state: GameState, position: Vec2): Tile | null => {
  const { chunk, tile } = absoluteToSplit.do(position);

  return state.map.chunkMap.get(chunk)?.get(tile) ?? null;
};

/**
 * Sets the tile at an absolute position. Assume the chunk exists.
 */
export const setTileAt = (
  state: GameState,
  position: Vec2,
  value: Tile | null
) => {
  const { chunk, tile } = absoluteToSplit.do(position);

  state.map.chunkMap.get(chunk)?.set(tile, value);
};

export const machineAt = (state: GameState, position: Vec2): Machine | null => {
  const tile = tileAt(state, position);

  if (!tile) return null;

  return tile.machine;
};

// ========== Helpers
export const addMachine = (machine: Machine) => {
  const { position, world } = machine;
  const split = absoluteToSplit.do(position);

  // Ensure chunk exists
  {
    const chunk = world.map.chunkMap.get(split.chunk);

    if (!chunk) return;
  }

  for (let i = 0; i < machine.size[0]; i++) {
    for (let j = 0; j < machine.size[1]; j++) {
      setTileAt(world, [position[0] + i, position[1] + j], {
        subTile: [i, j],
        machine,
      });
    }
  }

  world.emitter.emit("machineCreated", {
    machine,
  });
};

export const getOptions = <T extends ItemOptions[`type`]>(
  state: GameState,
  item: string,
  kind: T
): (ItemOptions & { type: T }) | null => {
  const config = state.items[item]?.options;

  if (config === undefined) return null;

  return config.type === kind ? (config as any) : null;
};

export const getStackSize = (state: GameState, item: Item) =>
  state.items[item].stackSize ?? 0;

// ========= Useful json serialization stuff
export const decodeTimedItem: Decoder<TimedItem> = decodeRecord({
  id: decodeString,
  birth: decodeNumber,
});

export const initialState = (
  ctx: CanvasRenderingContext2D,
  items: Record<string, ItemConfig>
) => {
  const world: GameState = {
    ctx,
    camera: {
      translation: [ctx.canvas.width / 2, ctx.canvas.height / 2],
      scale: 1,
    },
    keyboard: pressedKeys(),
    player: {
      position: [0, 0],
      rotation: 0,
      speedMultiplier: 3,
    },
    map: {
      chunkMap: new InfiniteMatrix(createChunkmapElement(() => world)),
    },
    mouse: {
      position: [0, 0],
    },
    items,
    tick: 0,
    time: 0,
    paused: false,
    pausedTimeDifference: 0,
    lastPausedAt: 0,
    emitter: new EventEmitter(),
  };

  return world;
};
