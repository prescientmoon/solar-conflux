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
import { renderIndicator } from "./render/mouseIndicator";
import { getHoveredTile } from "./utils/mouse";
import { addBelt, addBeltLike, BeltCurve, ConveyorBelt } from "./systems/belts";
import { Loader, Unloader } from "./systems/loaders";
import { Junction } from "./systems/junction";
import { Router } from "./systems/router";
import { Chest } from "./systems/chest";
import { tileAt } from "./systems/world";
import { debugFlags } from "./constants";

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
      [
        [
          [createChunk(), createChunk()],
          [createChunk(), createChunk()],
        ],
        [
          [createChunk(), createChunk()],
          [createChunk(), createChunk()],
        ],
      ],
      [
        [
          [createChunk(), createChunk()],
          [createChunk(), createChunk()],
        ],
        [
          [createChunk(), createChunk()],
          [createChunk(), createChunk()],
        ],
      ],
    ],
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
console.log(state.map.chunkMap[0][0]);
console.log(state.map.chunkMap[1][0]);
console.log(state.map.chunkMap[0][1]);
console.log(state.map.chunkMap[1][1]);

state.emitter.on("machineCreated", (d) => {
  addBeltLike(state, d.machine);
  addBelt(state, d.machine);
});

// ========== Demo scene
addMachine(new ConveyorBelt(state, Direction.Right, [3, 13], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [4, 13], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [3, 14], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [3, 15], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [4, 15], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [5, 15], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [5, 14], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [5, 13], "yellowBelt"));

addMachine(new ConveyorBelt(state, Direction.Up, [-3, -13], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [-4, -13], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [-3, -14], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [-3, -15], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [-4, -15], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [-5, -15], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [-5, -14], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [-5, -13], "yellowBelt"));

addMachine(new ConveyorBelt(state, Direction.Left, [-7, -13], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [-8, -13], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [-7, -14], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [-7, -15], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [-8, -15], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [-9, -15], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [-9, -14], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [-9, -13], "yellowBelt"));

addMachine(new Junction(state, [3, 4], item(`junction`)));

addMachine(new ConveyorBelt(state, Direction.Right, [3, 3], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [3, 5], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [4, 5], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [5, 5], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [5, 4], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [5, 3], "yellowBelt"));

addMachine(new ConveyorBelt(state, Direction.Down, [2, 2], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [2, 3], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [2, 4], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [4, 4], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [4, 2], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [3, 2], "yellowBelt"));

addMachine(new ConveyorBelt(state, Direction.Right, [10, 10], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [11, 10], "yellowBelt"));
addMachine(new Loader(state, Direction.Right, [12, 10], "yellowLoader"));
addMachine(new Chest(state, [13, 10], item(`woodChest`)));
addMachine(new Unloader(state, Direction.Right, [15, 10], `yellowUnloder`));
addMachine(new ConveyorBelt(state, Direction.Right, [16, 10], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [17, 10], "yellowBelt"));
addMachine(new Unloader(state, Direction.Right, [15, 11], `yellowUnloder`));
addMachine(new ConveyorBelt(state, Direction.Right, [16, 11], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [17, 11], "yellowBelt"));
addMachine(new Unloader(state, Direction.Down, [14, 12], `yellowUnloder`));
addMachine(new ConveyorBelt(state, Direction.Down, [14, 13], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [14, 14], "yellowBelt"));

addMachine(new Junction(state, [4, 3], item(`junction`)));

addMachine(new ConveyorBelt(state, Direction.Right, [0, 7], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [1, 7], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [2, 7], "yellowBelt"));
addMachine(new Router(state, [3, 7], item(`router`)));
addMachine(new ConveyorBelt(state, Direction.Right, [3, 6], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [3, 8], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [4, 7], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [5, 7], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [4, 6], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [5, 6], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [4, 8], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [5, 8], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [6, 6], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [6, 8], "yellowBelt"));
addMachine(new Router(state, [6, 7], item(`router`)));
addMachine(new ConveyorBelt(state, Direction.Right, [7, 7], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [8, 7], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [8, 8], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [8, 9], "yellowBelt"));

for (let i = 1; i < 9; i++) {
  addMachine(new ConveyorBelt(state, Direction.Left, [i, 10], "yellowBelt"));
}

addMachine(new ConveyorBelt(state, Direction.Up, [0, 8], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [0, 9], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [0, 10], "yellowBelt"));

addMachine(new Router(state, [9, 2], item(`distributor`)));
addMachine(new ConveyorBelt(state, Direction.Down, [9, 0], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [9, 1], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [10, 1], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [11, 1], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [11, 2], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [11, 3], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [11, 4], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [10, 4], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [9, 4], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [8, 4], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [8, 3], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [8, 2], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [7, 2], "yellowBelt"));

addMachine(new Chest(state, [14, 4], item(`woodBox`)));
addMachine(new Chest(state, [13, 6], item(`woodChest`)));
addMachine(new Chest(state, [16, 4], item(`woodWarehouse`)));

addMachine(new Router(state, [-1, -1], item(`distributor`)));
addMachine(new ConveyorBelt(state, Direction.Down, [-1, -2], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [-1, -3], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [-3, -1], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [-4, -1], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [-2, -1], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [0, -2], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [0, -3], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [1, -3], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [1, -2], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [1, -1], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Down, [1, 0], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [1, 1], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [0, 1], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Left, [-1, 1], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Up, [-2, 1], "yellowBelt"));
addMachine(new ConveyorBelt(state, Direction.Right, [-2, 0], "yellowBelt"));

const testBelts = [
  tileAt(state, [3, 3])?.machine,
  tileAt(state, [2, 2])?.machine,
  tileAt(state, [10, 10])?.machine,
  tileAt(state, [4, 13])?.machine,
  tileAt(state, [-3, -13])?.machine,
  tileAt(state, [-8, -15])?.machine,
  tileAt(state, [-1, -3])?.machine,
  tileAt(state, [0, 7])?.machine,
  tileAt(state, [9, 0])?.machine,
] as ConveyorBelt[];

for (const belt of testBelts) {
  belt.transportLine.items[0].push(
    ...Array(12)
      .fill(1)
      .map((_, index) => ({
        id: item("ironPlate"),
        position: index * 5,
      }))
  );
  belt.transportLine.items[1].push(
    ...Array(12)
      .fill(1)
      .map((_, index) => ({
        id: item("ironPlate"),
        position: index * 5,
      }))
  );
}

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

    for (const [tile] of allTiles(state)) {
      if (tile === null || tile.subTile[0] || tile.subTile[1]) continue;
      if (tile.machine instanceof ConveyorBelt)
        beltRenderer(state, tile.machine);
      else if (
        tile.machine instanceof Loader ||
        tile.machine instanceof Unloader
      )
        tile.machine.renderGround();
    }

    for (const [tile] of allTiles(state)) {
      if (tile === null || tile.subTile[0] || tile.subTile[1]) continue;
      if (tile.machine instanceof ConveyorBelt)
        beltItemRenderer(state, tile.machine);
      else if (
        tile.machine instanceof Loader ||
        tile.machine instanceof Unloader
      )
        tile.machine.renderItems();
    }

    for (const [tile] of allTiles(state)) {
      if (tile === null || tile.subTile[0] || tile.subTile[1]) continue;
      else if (
        tile.machine instanceof Junction ||
        tile.machine instanceof Router ||
        tile.machine instanceof Loader ||
        tile.machine instanceof Unloader ||
        tile.machine instanceof Chest
      )
        tile.machine.renderBuilding();
    }

    renderIndicator(getHoveredTile(state.mouse.position), state);
    if (debugFlags.showDebugText) renderDebugger(state);
    renderPlayer(state);
    ctx.resetTransform();
    requestAnimationFrame(main);
  } else {
    requestAnimationFrame(main);
    state.time = performance.now();
  }
};

// CAMERA ZOOMING
window.addEventListener("wheel", (e) => {
  if (state.camera.scale < 5 && e.deltaY < 0) {
    state.camera.scale -= e.deltaY / 1000;
  }
  if (state.camera.scale > 0.5 && e.deltaY > 0) {
    state.camera.scale -= e.deltaY / 1000;
  }
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

window.addEventListener("mousemove", (e) => {
  state.mouse.position = [
    e.clientX / state.camera.scale - state.camera.translation[0],
    e.clientY / state.camera.scale - state.camera.translation[1],
  ];
});

resize();
main();

// for debugging
// @ts-ignore
window.state = state;
