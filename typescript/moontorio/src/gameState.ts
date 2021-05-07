import type { KeyboardState } from "./keyboard";
import { chunkSize } from "./map";
import type { Player } from "./player";
import { TransportLine, TransportLineConfig } from "./systems/belts";
import { splitPosition } from "./systems/world";
import { Entity, ITransform, IUpdate } from "./utils/entity";
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
    id: Item;
    birth: number;
}

export type Machine = Entity & ITransform & IUpdate;

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

export type ItemOptions = TaggedUnion<{
    conveyorBelt: TransportLineConfig;
    loader: TransportLineConfig;
    junction: JunctionConfig;
    router: RouterConfig;
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
    // console.log(machine);

    const { position, world } = machine;
    const [chunkPos, subPos, chunkDirection] = splitPosition(position);

    // console.log({ chunkPos, subPos, chunkDirection });

    const chunk =
        world.map.chunkMap[chunkDirection[0]][chunkDirection[1]][chunkPos[0]][
            chunkPos[1]
        ];
    // console.log(chunk);

    if (!chunk) return;

    for (let i = 0; i < machine.size[0]; i++) {
        for (let j = 0; j < machine.size[1]; j++) {
            chunk[subPos[0] + i][subPos[1] + j] = {
                subTile: [i, j],
                machine,
            };
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
    const config = state.items[item].options;

    if (config === undefined) return null;

    return config.type === kind ? (config as any) : null;
};
