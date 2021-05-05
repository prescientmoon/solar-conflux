import { addMachine, Belt, GameState } from "./gameState";
import { pressedKeys } from "./keyboard";
import { createChunk } from "./map";
import { beltItemRenderer, beltRenderer } from "./render/belts";
import { renderPlayer, updatePlayer } from "./player";
import { Direction, Side } from "./utils/types";
import { item, items } from "./items";
import * as MoveBeltItems from "./systems/moveBeltItems";
import { allTiles } from "./utils/traversals";
import { isBelt, machineIs } from "./utils/machines";
import {
    updateLoader,
    loaderRenderer,
    loaderItemRenderer,
    implBeltForLoader,
} from "./systems/loaders";
import {
    implBeltForJunction,
    renderJunction,
    updateJunction,
} from "./systems/junction";
import { EventEmitter } from "./utils/events";
import { getBeltLength } from "./systems/beltCurving";

const canvas = document.getElementById("canvas")! as HTMLCanvasElement;
const ctx = canvas.getContext("2d")!;
const state: GameState = {
    ctx,
    camera: {
        translation: [canvas.width / 2, canvas.height / 2],
        scale: 20,
    },
    keyboard: pressedKeys(),
    player: {
        position: [0, 0],
        rotation: 0,
        speedMultiplier: 3,
    },
    map: {
        chunkMap: [
            [createChunk(), createChunk()],
            [createChunk(), createChunk()],
        ],
    },
    machineInterfaces: {
        belt: {
            beltLike: MoveBeltItems.implBeltForBelt,
        },
        junction: {
            beltLike: implBeltForJunction,
        },
        loader: {
            beltLike: implBeltForLoader,
        },
    },
    items,
    tick: 0,
    emitter: new EventEmitter(),
};

state.emitter.on("machineCreated", (d) => {
    MoveBeltItems.addBeltLike(state, d.machine, d.position);
    MoveBeltItems.addBelt(state, d.machine, d.position);
});

addMachine(state, [3, 4], {
    type: "junction",
    item: item("yellowJunction"),
    items: [
        [[], []],
        [[], []],
        [[], []],
        [[], []],
    ],
});

addMachine(state, [3, 3], MoveBeltItems.mkBelt(Direction.Down, "yellowBelt"));
addMachine(state, [3, 5], MoveBeltItems.mkBelt(Direction.Right, "yellowBelt"));
addMachine(state, [4, 5], MoveBeltItems.mkBelt(Direction.Right, "yellowBelt"));
addMachine(state, [5, 5], MoveBeltItems.mkBelt(Direction.Up, "yellowBelt"));
addMachine(state, [5, 4], MoveBeltItems.mkBelt(Direction.Up, "yellowBelt"));
addMachine(state, [5, 3], MoveBeltItems.mkBelt(Direction.Left, "yellowBelt"));

addMachine(state, [2, 2], MoveBeltItems.mkBelt(Direction.Down, "yellowBelt"));
addMachine(state, [2, 3], MoveBeltItems.mkBelt(Direction.Down, "yellowBelt"));
addMachine(state, [2, 4], MoveBeltItems.mkBelt(Direction.Right, "yellowBelt"));
addMachine(state, [4, 4], MoveBeltItems.mkBelt(Direction.Up, "yellowBelt"));
addMachine(state, [4, 2], MoveBeltItems.mkBelt(Direction.Left, "yellowBelt"));
addMachine(state, [3, 2], MoveBeltItems.mkBelt(Direction.Left, "yellowBelt"));

addMachine(state, [4, 3], {
    type: "junction",
    item: item("yellowJunction"),
    items: [
        [[], []],
        [[], []],
        [[], []],
        [[], []],
    ],
});

addMachine(state, [7, 5], MoveBeltItems.mkBelt(Direction.Up, "yellowBelt"));
addMachine(state, [8, 5], MoveBeltItems.mkBelt(Direction.Left, "yellowBelt"));
addMachine(state, [8, 4], MoveBeltItems.mkBelt(Direction.Down, "yellowBelt"));
addMachine(state, [7, 4], MoveBeltItems.mkBelt(Direction.Right, "yellowBelt"));

addMachine(state, [10, 4], MoveBeltItems.mkBelt(Direction.Down, "yellowBelt"));
addMachine(state, [10, 5], MoveBeltItems.mkBelt(Direction.Down, "yellowBelt"));
addMachine(state, [10, 6], MoveBeltItems.mkBelt(Direction.Down, "yellowBelt"));

addMachine(state, [10, 7], {
    type: "loader",
    direction: Direction.Down,
    item: item("yellowLoader"),
    items: [[], []],
});

const testBelts = [
    state.map.chunkMap[0][0]![3][3],
    state.map.chunkMap[0][0]![7][5],
    state.map.chunkMap[0][0]![10][4],
] as Belt[];

for (const belt of testBelts) {
    belt.machine.items[0].push(
        ...Array(10)
            .fill(1)
            .map((_, index) => ({
                item: item("ironPlate"),
                position: index * 5,
            }))
    );
    belt.machine.items[1].push(
        ...Array(10)
            .fill(1)
            .map((_, index) => ({
                item: item("ironPlate"),
                position: index * 5,
            }))
    );
}

const testBelt2 = state.map.chunkMap[0][0]![2][2] as Belt;

testBelt2.machine.items[0].push(
    ...Array(10)
        .fill(1)
        .map((_, index) => ({
            item: item("ironPlate"),
            position: index * 5,
        }))
);
testBelt2.machine.items[1].push(
    ...Array(10)
        .fill(1)
        .map((_, index) => ({
            item: item("ironPlate"),
            position: index * 5,
        }))
);

// console.log(state);

ctx.imageSmoothingEnabled = false;
const adjustCamera = () => {
    state.camera.translation[0] = Math.floor(
        canvas.width / 2 - state.player.position[0]
    );
    state.camera.translation[1] = Math.floor(
        canvas.height / 2 - state.player.position[1]
    );
};

const resize = () => {
    const width = window.innerWidth;
    const height = window.innerHeight;

    canvas.height = height;
    canvas.width = width;

    canvas.style.width = String(width);
    canvas.style.height = String(height);
};

const clear = () => {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
};

const main = () => {
    clear();

    state.tick++;

    // Update stage:
    updatePlayer(state);

    for (const [tile, position] of allTiles(state)) {
        if (tile === null) continue;
        if (isBelt(tile)) MoveBeltItems.updteBelt(state, tile, position);
        if (machineIs("loader", tile)) updateLoader(state, tile);
        if (machineIs("junction", tile)) updateJunction(state, tile, position);
    }

    // Actual rendering:
    ctx.translate(state.camera.translation[0], state.camera.translation[1]);

    for (const [tile, position] of allTiles(state)) {
        if (tile === null) continue;
        if (isBelt(tile)) beltRenderer(state, tile, position);
        else if (machineIs("loader", tile))
            loaderRenderer(state, tile, position);
    }

    for (const [tile, position] of allTiles(state)) {
        if (tile === null) continue;
        if (isBelt(tile)) beltItemRenderer(state, tile, position);
        else if (machineIs("loader", tile))
            loaderItemRenderer(state, tile, position);
    }

    for (const [tile, position] of allTiles(state)) {
        if (tile === null) continue;
        else if (machineIs("junction", tile)) renderJunction(state, position);
    }

    renderPlayer(state);
    adjustCamera();

    ctx.resetTransform();

    requestAnimationFrame(main);
};

window.onresize = resize;
resize();
main();
