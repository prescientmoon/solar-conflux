import { Mat23Like, scale23, transform23 } from "@thi.ng/matrices";
import { Belt, GameState, loadAsset } from "./gameState";
import { pressedKeys } from "./keyboard";
import { createChunk } from "./map";
import { beltitemRenderer, beltRenderer } from "./render/belts";
import { renderPlayer, updatePlayer } from "./player";
import { Direction } from "./utils/types";
import { item, items } from "./items";
import * as MoveBeltItems from "./systems/moveBeltItems";
import { replicate } from "./utils/array";
import { allTiles } from "./utils/traversals";
import { isBelt, machineIs } from "./utils/machines";
import { renderSimpleTile } from "./render/simpleTile";

const canvas = document.getElementById("canvas")! as HTMLCanvasElement;
const ctx = canvas.getContext("2d")!;
const state: GameState = {
  ctx,
  camera: {
    translation: [0, 0],
    scale: 20,
  },
  keyboard: pressedKeys(),
  player: {
    position: [0, 0],
    rotation: 0,
  },
  map: {
    chunkMap: [
      [createChunk(), createChunk()],
      [createChunk(), createChunk()],
    ],
    outputBelts: [],
    allBelts: [],
  },
  items,
};

MoveBeltItems.addBelt(state, [3, 3], Direction.Down, item("yellowBelt"));
MoveBeltItems.addBelt(state, [3, 4], Direction.Down, item("yellowBelt"));
MoveBeltItems.addBelt(state, [3, 5], Direction.Right, item("yellowBelt"));
MoveBeltItems.addBelt(state, [4, 5], Direction.Up, item("yellowBelt"));
MoveBeltItems.addBelt(state, [4, 4], Direction.Up, item("yellowBelt"));
MoveBeltItems.addBelt(state, [4, 3], Direction.Left, item("yellowBelt"));

MoveBeltItems.addBelt(state, [7, 5], Direction.Up, item("yellowBelt"));
MoveBeltItems.addBelt(state, [8, 5], Direction.Left, item("yellowBelt"));
MoveBeltItems.addBelt(state, [8, 4], Direction.Down, item("yellowBelt"));
MoveBeltItems.addBelt(state, [7, 4], Direction.Right, item("yellowBelt"));

MoveBeltItems.addBelt(state, [10, 4], Direction.Down, item("yellowBelt"));
MoveBeltItems.addBelt(state, [10, 5], Direction.Down, item("yellowBelt"));
MoveBeltItems.addBelt(state, [10, 6], Direction.Down, item("yellowBelt"));
MoveBeltItems.addBelt(state, [9, 7], Direction.Down, item("yellowBelt"));
MoveBeltItems.addBelt(state, [11, 7], Direction.Down, item("yellowBelt"));
MoveBeltItems.addBelt(state, [10, 7], Direction.Down, item("distributor"));

const testBelts = [
  state.map.chunkMap[0][0]![3][3],
  state.map.chunkMap[0][0]![7][5],
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

console.log(state);

ctx.imageSmoothingEnabled = false;

const resize = () => {
  const width = window.innerWidth;
  const height = window.innerHeight;

  canvas.width = width;
  canvas.height = height;

  canvas.style.width = String(width);
  canvas.style.height = String(height);
};

const clear = () => {
  ctx.clearRect(0, 0, 1000, 1000);
};

const main = () => {
  clear();
  updatePlayer(state);
  MoveBeltItems.updateItems(state);

  ctx.translate(state.camera.translation[0], state.camera.translation[1]);

  for (const [tile, position] of allTiles(state)) {
    if (tile === null) continue;
    if (isBelt(tile)) beltRenderer(state, tile, position);
    else {
      renderSimpleTile(state, tile, position);
    }
  }

  beltitemRenderer.render(state);
  renderPlayer(state);

  ctx.resetTransform();

  requestAnimationFrame(main);
};

resize();
main();
