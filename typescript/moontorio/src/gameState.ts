import type { KeyboardState } from "./keyboard";
import type { Player } from "./player";
import { TransportLineConfig } from "./systems/belts";
import { setTileAt, splitPosition } from "./systems/world";
import { Entity, ITransform, IUpdate } from "./utils/entity";
import { EventEmitter } from "./utils/events";
import { IToJson } from "./utils/json";
import type { Nullable, TaggedUnion, Vec2 } from "./utils/types";

export type Item = string;

export interface TimedItem {
  id: Item;
  birth: number;
}

export type Machine = Entity & ITransform & IUpdate & IToJson;

export type Tile = {
  subTile: Vec2;
  machine: Machine;
};

export type Chunk = Nullable<Tile>[][];

export type DirectionChunkMatrixes = Nullable<Chunk>[][];

export interface GameMap {
  chunkMap: DirectionChunkMatrixes[][];
}

export interface JunctionConfig {
  delay: number;
  capacity: number;
}

export interface RouterConfig extends JunctionConfig {
  /** Size in tiles a side of the router should take.
   * Eg: a size of 2 will create a 2x2 tile
   */
  size: number;
}

export interface ChestConfig {
  slots: number;
  size: number;
}

export type ItemOptions = TaggedUnion<{
  conveyorBelt: TransportLineConfig;
  loader: TransportLineConfig;
  unloader: TransportLineConfig;
  junction: JunctionConfig;
  router: RouterConfig;
  chest: ChestConfig;
}>;

export interface ItemConfig {
  texture: Image;
  stackSize: number;
  tileTexture?: Image;
  options?: ItemOptions;
}

export interface GameEvents {
  machineCreated: {
    machine: Machine;
  };
}

export interface Mouse {
  position: [number, number];
}

export interface GameState {
  ctx: CanvasRenderingContext2D;
  camera: {
    translation: [number, number];
    scale: number;
  };
  keyboard: KeyboardState;
  player: Player;
  map: GameMap;
  items: Record<Item, ItemConfig>;
  mouse: Mouse;
  tick: number;
  time: number;
  paused: boolean;
  pausedTimeDifference: number;
  lastPausedAt: number;
  emitter: EventEmitter<GameEvents>;
}

export type Renderer = {
  render: (state: GameState) => void;
  z: number;
};

// ========== Asset stuff
export type Image = HTMLImageElement;
