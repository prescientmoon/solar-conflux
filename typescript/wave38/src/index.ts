import { Mat23Like, scale23, transform23 } from "@thi.ng/matrices";
import { GameState } from "./gameState";
import { pressedKeys } from "./keyboard";
import { renderPlayer, updatePlayer } from "./player";

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
  },
};

ctx.imageSmoothingEnabled = false;

const resize = () => {
  const width = Math.floor(window.innerWidth / state.camera.scale);
  const height = Math.floor(window.innerHeight / state.camera.scale);

  canvas.width = width;
  canvas.height = height;

  canvas.style.width = `${width * state.camera.scale}px`;
  canvas.style.height = `${height * state.camera.scale}px`;
};

const clear = () => {
  ctx.clearRect(0, 0, 1000, 1000);
};

const main = () => {
  clear();
  updatePlayer(state);

  ctx.translate(state.camera.translation[0], state.camera.translation[1]);
  ctx.rotate(0.1);

  renderPlayer(state);

  ctx.resetTransform();

  requestAnimationFrame(main);
};

resize();
main();
