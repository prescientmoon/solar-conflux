import { addMachine, GameState } from "./gameState";
import { pressedKeys } from "./keyboard";
import { createChunk } from "./map";
import { beltItemRenderer, beltRenderer } from "./render/belts";
import { renderPlayer, updatePlayer } from "./player";
import { Direction, Side } from "./utils/types";
import { item, items } from "./items";
import { allTiles } from "./utils/traversals";
import { EventEmitter } from "./utils/events";

import { renderDebugger } from "./render/debugScreen";
import { addBelt, addBeltLike, ConveyorBelt } from "./systems/belts";
import { Loader } from "./systems/loaders";

export const canvas = document.getElementById("canvas")! as HTMLCanvasElement;
const ctx = canvas.getContext("2d")!;
const state: GameState = {
  ctx,
  camera: {
    translation: [canvas.width / 2, canvas.height / 2],
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
      [createChunk(), createChunk()],
      [createChunk(), createChunk()],
    ],
  },
  items,
  tick: 0,
  time: 0,
  paused: false,
  pausedTimeDifference: 0,
  lastPausedAt: 0,
  emitter: new EventEmitter(),
};

state.emitter.on("machineCreated", (d) => {
  addBeltLike(state, d.machine, d.position);
  addBelt(state, d.machine, d.position);
});

/* addMachine(state, [3, 4], {
  type: "junction",
  item: item("yellowJunction"),
  items: [
    [[], []],
    [[], []],
    [[], []],
    [[], []],
  ],
}); */

addMachine(new ConveyorBelt(state, Direction.Down, [3, 3], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [3, 5], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [4, 5], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [5, 5], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [5, 4], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [5, 3], "yellowBelt"));

addMachine(new ConveyorBelt(state, Direction.Down, [2, 2], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [2, 3], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [2, 4], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [4, 4], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [4, 2], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [3, 2], "yellowBelt"));

addMachine(new ConveyorBelt(state, Direction.Right, [10, 10], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [11, 10], "yellowBelt"));
addMachine(new Loader(state, Direction.Right, [12, 10], "yellowLoader"));

/* addMachine(state, [4, 3], {
  type: "junction",
  item: item("yellowJunction"),
  items: [
    [[], []],
    [[], []],
    [[], []],
    [[], []],
  ],
}); */

/* addMachine(state, [7, 5], MoveBeltItems.mkBelt(Direction.Up, "yellowBelt"));
addMachine(state, [8, 5], MoveBeltItems.mkBelt(Direction.Left, "yellowBelt"));
addMachine(state, [8, 4], MoveBeltItems.mkBelt(Direction.Down, "yellowBelt"));
addMachine(state, [7, 4], MoveBeltItems.mkBelt(Direction.Right, "yellowBelt"));

addMachine(state, [10, 4], MoveBeltItems.mkBelt(Direction.Down, "yellowBelt"));
addMachine(state, [10, 5], MoveBeltItems.mkBelt(Direction.Down, "yellowBelt"));
addMachine(state, [10, 6], MoveBeltItems.mkBelt(Direction.Down, "yellowBelt")); */

/* addMachine(state, [10, 7], {
  type: "loader",
  direction: Direction.Down,
  item: item("yellowLoader"),
  items: [[], []],
}); */

const testBelts = [
  state.map.chunkMap[0][0]![3][3]?.machine,
  state.map.chunkMap[0][0]![2][2]?.machine,
  state.map.chunkMap[0][0]![10][10]?.machine,
] as ConveyorBelt[];

for (const belt of testBelts) {
  belt.transportLine.items[0].push(
    ...Array(10)
      .fill(1)
      .map((_, index) => ({
        id: item("ironPlate"),
        position: index * 5,
      }))
  );
  belt.transportLine.items[1].push(
    ...Array(10)
      .fill(1)
      .map((_, index) => ({
        id: item("ironPlate"),
        position: index * 5,
      }))
  );
}

// console.log(state);

ctx.imageSmoothingEnabled = false;
const adjustCamera = () => {
  state.ctx.scale(state.camera.scale, state.camera.scale);
  state.camera.translation[0] =
    Math.floor(
      (canvas.width / (2 * state.camera.scale) - state.player.position[0]) *
        1000
    ) / 1000;
  state.camera.translation[1] =
    Math.floor(
      (canvas.height / (2 * state.camera.scale) - state.player.position[1]) *
        1000
    ) / 1000;
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
  if (!state.paused) {
    clear();

    state.tick++;
    state.time = performance.now() - state.pausedTimeDifference;

    // Update stage:
    updatePlayer(state);
    adjustCamera();

    for (const [tile, position] of allTiles(state)) {
      if (tile === null) continue;
      tile.machine.update();
    }

    // Actual rendering:
    ctx.translate(state.camera.translation[0], state.camera.translation[1]);

    for (const [tile, position] of allTiles(state)) {
      if (tile === null) continue;
      if (tile.machine instanceof ConveyorBelt)
        beltRenderer(state, tile.machine);
      else if (tile.machine instanceof Loader) tile.machine.renderGround();
    }

    for (const [tile, position] of allTiles(state)) {
      if (tile === null) continue;
      if (tile.machine instanceof ConveyorBelt)
        beltItemRenderer(state, tile.machine);
      if (tile.machine instanceof Loader) tile.machine.renderBuilding();
    }

    for (const [tile, position] of allTiles(state)) {
      if (tile === null) continue;
      // else if (machineIs("junction", tile)) renderJunction(state, position);
    }

    renderPlayer(state);
    renderDebugger(state);
    ctx.resetTransform();
    requestAnimationFrame(main);
  } else {
    requestAnimationFrame(main);
    state.time = performance.now();
  }
};
// CAMERA ZOOMING
window.addEventListener("wheel", (e) => {
  if (state.camera.scale < 5 && e.deltaY < 0)
    state.camera.scale -= e.deltaY / 1000;
  if (state.camera.scale > 0.5 && e.deltaY > 0)
    state.camera.scale -= e.deltaY / 1000;
});
window.onresize = resize;

window.onblur = () => {
  state.paused = true;
  state.lastPausedAt = performance.now();
  console.log("paused");
};

window.onfocus = () => {
  if (state.paused) {
    state.paused = false;
    state.pausedTimeDifference += performance.now() - state.lastPausedAt;
    console.log("unpaused");
  }
};

resize();
main();
