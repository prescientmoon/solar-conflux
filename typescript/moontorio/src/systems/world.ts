import type {
  GameState,
  Machine,
  Tile,
  ItemOptions,
  Item,
  TimedItem,
} from "../gameState";
import { pressedKeys } from "../keyboard";
import { chunkSize } from "../map";
import type { Entity } from "../utils/entity";
import { EventEmitter } from "../utils/events";
import {
  decodeNumber,
  Decoder,
  decodeRecord,
  decodeString,
} from "../utils/json";
import type { Vec2 } from "../utils/types";

export const splitPosition = (position: Vec2): [Vec2, Vec2, Vec2] => [
  [
    Math.abs(Math.floor(position[0] / chunkSize)),
    Math.abs(Math.floor(position[1] / chunkSize)),
  ],
  [Math.abs(position[0] % chunkSize), Math.abs(position[1] % chunkSize)],
  [position[0] >= 0 ? 0 : 1, position[1] >= 0 ? 0 : 1],
];

export const tileAt = (state: GameState, position: Vec2): Tile | null =>
  state.map.chunkMap[position[0] >= 0 ? 0 : 1][position[1] >= 0 ? 0 : 1][
    Math.abs(Math.floor(position[0] / chunkSize))
  ][Math.abs(Math.floor(position[1] / chunkSize))]?.[
    Math.abs(position[0] % chunkSize)
  ][Math.abs(position[1] % chunkSize)] ?? null;

/**
 * Sets the tile at an absolute position. Assume the chunk exists.
 */
export const setTileAt = (
  state: GameState,
  position: Vec2,
  tile: Tile | null
) => {
  state.map.chunkMap[position[0] >= 0 ? 0 : 1][position[1] >= 0 ? 0 : 1][
    Math.abs(Math.floor(position[0] / chunkSize))
  ][Math.abs(Math.floor(position[1] / chunkSize))]![
    Math.abs(position[0] % chunkSize)
  ][Math.abs(position[1] % chunkSize)] = tile;
};

export const machineAt = (state: GameState, position: Vec2): Machine | null => {
  const tile = tileAt(state, position);

  if (!tile) return null;

  return tile.machine;
};

// ========== Helpers
export const addMachine = (machine: Machine) => {
  const { position, world } = machine;

  // Ensure chunk exists
  {
    const [chunkPos, , chunkDirection] = splitPosition(position);

    const chunk =
      world.map.chunkMap[chunkDirection[0]][chunkDirection[1]][chunkPos[0]][
        chunkPos[1]
      ];

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

export const initialState = (ctx: CanvasRenderingContext2D): GameState => ({
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
    chunkMap: [
      [[], []],
      [[], []],
    ],
  },
  mouse: {
    position: [0, 0],
  },
  items: {},
  tick: 0,
  time: 0,
  paused: false,
  pausedTimeDifference: 0,
  lastPausedAt: 0,
  emitter: new EventEmitter(),
});
