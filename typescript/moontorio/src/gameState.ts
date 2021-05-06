import type { KeyboardState } from "./keyboard";
import { chunkSize } from "./map";
import type { Player } from "./player";
import { EventEmitter } from "./utils/events";
import type {
    TaggedUnion,
    Direction,
    Nullable,
    Pair,
    Side,
    Vec2,
    Directional,
} from "./utils/types";

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

export type TransportLine<T = BeltItem> = Pair<T[]>;

export interface TimedItem {
    item: Item;
    birth: number;
}

type MachineImpl = TaggedUnion<{
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
        items: Pair<Array<TimedItem>>;
    };
    junction: {
        items: Directional<TransportLine<TimedItem>>;
    };
    chest: {
        storage: Storage<Nullable<Item>>;
    };
}> & { item: Item };

export type Machine<
    T extends MachineImpl["type"] = MachineImpl["type"]
> = MachineImpl & { type: T };

export type Tile<T extends MachineImpl["type"] = MachineImpl["type"]> = {
    subTile: Vec2;
    machine: Machine<T>;
};

// Aliases for different machines
export type Belt = Tile<"belt">;
export type Loader = Tile<"loader">;
export type Junction = Tile<"junction">;

export type Chunk = Nullable<Tile>[][];

export interface GameMap {
    chunkMap: Nullable<Chunk>[][];
}

export interface BeltLikePushArgumens<T> {
    state: GameState;
    position: Vec2;
    belt: T;
    item: BeltItem;
    side: Side;
    direction: Direction;
}

export interface MachineComponents<T> {
    beltLike: {
        // This generic is a buggy workaround
        // typescript' lack of existential types
        push: (args: BeltLikePushArgumens<T>) => boolean;
        outputs(self: T): Array<Direction>;
    };
}

export interface ItemConfig {
    texture: Image;
    stackSize: number;
    tileTexture?: Image;
}

export interface GameEvents {
    machineCreated: {
        machine: Machine;
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
    machineInterfaces: Partial<
        {
            [T in Machine["type"]]: MachineComponents<Tile<T>>;
        }
    >;
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
export const getComponent = <
    M extends MachineImpl["type"] = MachineImpl["type"],
    K extends keyof MachineComponents<Tile<M>> = keyof MachineComponents<
        Tile<M>
    >
>(
    state: GameState,
    machine: M,
    componentName: K
): MachineComponents<Tile<M>>[K] | null => {
    const component = state.machineInterfaces[machine]?.[componentName];

    return (component as MachineComponents<Tile<M>>[K] | undefined) ?? null;
};

export const splitPosition = (position: Vec2): Pair<Vec2> => [
    [Math.floor(position[0] / chunkSize), Math.floor(position[1] / chunkSize)],
    [position[0] % chunkSize, position[1] % chunkSize],
];

export const addMachine = (
    state: GameState,
    position: Vec2,
    machine: Machine
) => {
    const [chunkPos, subPos] = splitPosition(position);

    state.map.chunkMap[chunkPos[0]][chunkPos[1]]![subPos[0]][subPos[1]] = {
        subTile: [0, 0],
        machine,
    };

    state.emitter.emit("machineCreated", {
        position,
        machine,
    });
};
