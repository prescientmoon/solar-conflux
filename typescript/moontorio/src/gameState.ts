import type { KeyboardState } from "./keyboard";
import type { Player } from "./player";
import type {
  ADT,
  Direction,
  Neighbour,
  Nullable,
  Pair,
  Vec2,
} from "./utils/types";

export type Item = number;

export type Machine = ADT<{
  belt: {
    direction: Direction;
    inputs: Direction[];
    items: Array<{
      item: Item;
      position: number;
    }>;
  };
}> & { item: Item };

export type Tile = {
  subTile: Vec2;
  machine: Machine;
};

export type Chunk = Nullable<Tile>[][];

export interface GameMap {
  chunkMap: Nullable<Chunk>[][];
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
  settings: {
    tileSize: number;
  };
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

    console.log(result);
  };

  imageMap.set(src, result);

  return result;
};
