import { Mat23Like, scale23, transform23 } from "@thi.ng/matrices";
import { GameState } from "./gameState";
import { pressedKeys } from "./keyboard";
import { createChunk } from "./map";
import { beltRenderer } from "./render/belts";
import { renderPlayer, updatePlayer } from "./player";
import { Direction } from "./utils/types";

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
  },
  settings: {
    tileSize: 50,
  },
};

state.map.chunkMap[0][0]![3][3] = {
  subTile: [0, 0],
  machine: {
    type: "belt",
    direction: Direction.Down,
    item: 0,
    items: [],
    inputs: [],
  },
};
state.map.chunkMap[0][0]![3][4] = {
  subTile: [0, 0],
  machine: {
    type: "belt",
    direction: Direction.Down,
    item: 0,
    items: [],
    inputs: [Direction.Up],
  },
};
state.map.chunkMap[0][0]![3][5] = {
  subTile: [0, 0],
  machine: {
    type: "belt",
    direction: Direction.Right,
    item: 0,
    items: [],
    inputs: [Direction.Up],
  },
};
state.map.chunkMap[0][0]![7][5] = {
  subTile: [0, 0],
  machine: {
    type: "belt",
    direction: Direction.Right,
    item: 0,
    items: [],
    inputs: [Direction.Up],
  },
};
state.map.chunkMap[0][0]![8][5] = {
  subTile: [0, 0],
  machine: {
    type: "belt",
    direction: Direction.Up,
    item: 0,
    items: [],
    inputs: [Direction.Left],
  },
};
state.map.chunkMap[0][0]![8][4] = {
  subTile: [0, 0],
  machine: {
    type: "belt",
    direction: Direction.Left,
    item: 0,
    items: [],
    inputs: [Direction.Down],
  },
};
state.map.chunkMap[0][0]![7][4] = {
  subTile: [0, 0],
  machine: {
    type: "belt",
    direction: Direction.Down,
    item: 0,
    items: [],
    inputs: [Direction.Right],
  },
};

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

  ctx.translate(state.camera.translation[0], state.camera.translation[1]);

  beltRenderer.render(state);
  renderPlayer(state);

  ctx.resetTransform();

  requestAnimationFrame(main);
};

resize();
main();
