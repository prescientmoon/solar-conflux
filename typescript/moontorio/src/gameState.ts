import type { KeyboardState } from "./keyboard";
import type { Player } from "./player";
import type { ADT, Direction, Nullable, Pair, Side, Vec2 } from "./utils/types";

export type Item = string;

export interface StorageSlot<T = Item> {
  item: T;
  amount: number;
}

export type Storage<T> = Array<StorageSlot<T>>;

export interface BeltItem {
  // This is a pair because we have 2 lanes.
  item: Item;
  position: number;
}

export interface TimedItem {
  item: Item;
  elapsed: number;
}

export type Machine = ADT<{
  belt: {
    direction: Direction;
    inputs: Direction[];
    items: Pair<Array<BeltItem>>;
  };
  loader: {
    direction: Direction;
    items: Pair<Array<BeltItem>>;
  };
  unloader: {
    direction: Direction;
    items: Storage<TimedItem>;
  };
  distributor: {
    items: Pair<
      Array<{
        item: Item;
        elapsed: TimedItem;
      }>
    >;
  };
  chest: {
    storage: Storage<Nullable<Item>>;
  };
}> & { item: Item };

export type Tile<T extends Machine["type"] = Machine["type"]> = {
  subTile: Vec2;
  machine: Machine & { type: T };
};

// Aliases for different machines
export type Belt = Tile<"belt">;
export type Loader = Tile<"loader">;

export type Chunk = Nullable<Tile>[][];

export interface GameMap {
  chunkMap: Nullable<Chunk>[][];
  outputBelts: Vec2[];

  // TODO: find a better way to handle cycles
  allBelts: Vec2[];
}

export interface ItemComponents<T = Machine> {
  beltLike: {
    // This generic is a buggy workaround
    // typescript' lack of existential types
    push: <P extends T>(args: {
      state: GameState;
      position: Vec2;
      belt: P;
      item: BeltItem;
      side: Side;
    }) => boolean;
  };
}

export interface ItemConfig {
  texture: Image;
  stackSize: number;
  components?: Partial<ItemComponents>;
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
}

export type Renderer = {
  render: (state: GameState) => void;
  z: number;
};

// ========== Asset stuff
export type Image = HTMLImageElement;

let imageMap = new Map<string, Image>();

export const loadAsset = (src: string): Image => {
  if (imageMap.has(src)) return imageMap.get(src)!;

  const result = new Image();
  result.src = src;

  result.onload = () => {
    result.height = result.naturalHeight;
    result.width = result.naturalWidth;
  };

  imageMap.set(src, result);

  return result;
};

// ========== Helpers
export const getComponent = <
  T = Tile,
  K extends keyof ItemComponents<T> = keyof ItemComponents<T>
>(
  state: GameState,
  item: string,
  componentName: K
): ItemComponents<T>[K] | null => {
  const component = state.items[item]?.components?.[componentName];

  return (component as ItemComponents<T>[K] | undefined) ?? null;
};
