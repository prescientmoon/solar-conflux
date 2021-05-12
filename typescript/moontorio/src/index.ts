import { debugFlags, screenHeight } from "./settings";
import { loadAsset } from "./utils/assets";
import { ecs, Env, onEntityCreated } from "./ecs";
import * as RenderGroundAnimation from "./systems/render/groundAnimation";
import { Direction } from "./utils/direction";
import {
  beltCurvedLeft,
  beltCurvedRight,
  straightBelt,
} from "./utils/assets/beltAnimations";
import { TransportLineSystem } from "./systems/transportLines";
import { showTransportLines } from "./systems/render/debugTransportLines";

const canvas = document.getElementById(`canvas`) as HTMLCanvasElement;
const ctx = canvas.getContext(`2d`)!;

const env: Env = {
  tick: 0,
  ctx,
};

const transportLineSystem = new TransportLineSystem();
onEntityCreated(ecs, (id) => transportLineSystem.onEntityCreated(id));

ecs.defEntity({
  groundAnimation: straightBelt,
  position: new Int32Array([0, 0]),
  direction: { direction: Direction.Right },
  transportLine: {
    id: null,
  },
});

ecs.defEntity({
  groundAnimation: straightBelt,
  position: new Int32Array([1, 0]),
  direction: { direction: Direction.Right },
  transportLine: { id: null },
});
ecs.defEntity({
  groundAnimation: beltCurvedRight,
  position: new Int32Array([2, 0]),
  direction: { direction: Direction.Down },
  transportLine: { id: null },
});
ecs.defEntity({
  groundAnimation: beltCurvedLeft,
  position: new Int32Array([2, 1]),
  direction: { direction: Direction.Right },
  transportLine: { id: null },
});
ecs.defEntity({
  groundAnimation: straightBelt,
  position: new Int32Array([3, 1]),
  direction: { direction: Direction.Right },
  transportLine: { id: null },
});
ecs.defEntity({
  groundAnimation: straightBelt,
  position: new Int32Array([8, 1]),
  direction: { direction: Direction.Left },
  transportLine: { id: null },
});
ecs.defEntity({
  groundAnimation: straightBelt,
  position: new Int32Array([7, 1]),
  direction: { direction: Direction.Left },
  transportLine: { id: null },
});
ecs.defEntity({
  groundAnimation: straightBelt,
  position: new Int32Array([6, 1]),
  direction: { direction: Direction.Left },
  transportLine: { id: null },
});

// ========= Rendering stuff
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

const main = () => {
  env.tick++;

  clear();

  RenderGroundAnimation.update(env);

  if (debugFlags.showTransportLines)
    showTransportLines(env, transportLineSystem);

  requestAnimationFrame(main);
};

main();
