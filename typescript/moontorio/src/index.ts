import { screenHeight } from "./settings";
import { loadAsset } from "./utils/assets";
import { ecs, Env } from "./ecs";
import * as RenderGroundAnimation from "./systems/render/groundAnimation";
import { Direction } from "./utils/direction";
import {
  beltCurvedLeft,
  beltCurvedRight,
  straightBelt,
} from "./utils/assets/beltAnimations";

const canvas = document.getElementById(`canvas`) as HTMLCanvasElement;
const ctx = canvas.getContext(`2d`)!;

const spritesheet = loadAsset(`assets/belts.png`);

ecs.defEntity({
  groundAnimation: straightBelt,
  position: new Int32Array([0, 0]),
  direction: Direction.Right,
});

ecs.defEntity({
  groundAnimation: straightBelt,
  position: new Int32Array([1, 0]),
  direction: Direction.Right,
});

ecs.defEntity({
  groundAnimation: beltCurvedRight,
  position: new Int32Array([2, 0]),
  direction: Direction.Down,
});

ecs.defEntity({
  groundAnimation: straightBelt,
  position: new Int32Array([2, 1]),
  direction: Direction.Down,
});
ecs.defEntity({
  groundAnimation: beltCurvedLeft,
  position: new Int32Array([2, 1]),
  direction: Direction.Right,
});

const resize = () => {
  const wheight = window.innerHeight;
  const wwidth = window.innerWidth;

  const width = Math.floor((wwidth * screenHeight) / wheight);

  canvas.width = width;
  canvas.height = screenHeight;

  canvas.style.height = `${wheight}px`;
  canvas.style.width = `${wwidth}px`;
};

const clear = () => {
  ctx.clearRect(0, 0, 1000, 1000);
};

resize();

const env: Env = {
  tick: 0,
  ctx,
};

const main = () => {
  env.tick++;

  clear();

  RenderGroundAnimation.update(env);

  requestAnimationFrame(main);
};

main();
