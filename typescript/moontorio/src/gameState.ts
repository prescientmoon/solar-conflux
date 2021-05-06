import type { KeyboardState } from "./keyboard";
import { chunkSize } from "./map";
import type { Player } from "./player";
import { TransportLineConfig } from "./systems/belts";
import { splitPosition } from "./systems/world";
import { Entity, IPosition } from "./utils/entity";
import { EventEmitter } from "./utils/events";
import type {
    Direction,
    Nullable,
    Pair,
    Side,
    TaggedUnion,
    Vec2,
} from "./utils/types";

export type Item = string;

export interface TimedItem {
    item: Item;
    birth: number;
}

export type Machine = Entity & IPosition;

export type Tile = {
    subTile: Vec2;
    machine: Machine;
};

export type Chunk = Nullable<Tile>[][];

export interface GameMap {
    chunkMap: Nullable<Chunk>[][];
}

export type ItemOptions = TaggedUnion<{
    conveyorBelt: TransportLineConfig;
}>;

export interface ItemConfig {
    texture: Image;
    stackSize: number;
    tileTexture?: Image;
    options?: ItemOptions;
}

export interface GameEvents {
    machineCreated: {
        machine: Entity;
        position: Vec2;
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
export const addMachine = (machine: Machine) => {
    const { position, world } = machine;
    const [chunkPos, subPos] = splitPosition(position);

    world.map.chunkMap[chunkPos[0]][chunkPos[1]]![subPos[0]][subPos[1]] = {
        subTile: [0, 0],
        machine,
    };

    world.emitter.emit("machineCreated", {
        position,
        machine,
    });
};

export const getOptions = <T extends ItemOptions[`type`]>(
    state: GameState,
    item: string,
    kind: T
): (ItemOptions & { type: T }) | null => {
    const config = state.items[item].options;

    if (config === undefined) return null;

    return config.type === kind ? (config as any) : null;
};
