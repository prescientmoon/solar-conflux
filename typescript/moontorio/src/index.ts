import { debugFlags, screenHeight } from "./settings";
import { loadAsset } from "./utils/assets";
import { Components, ecs, Env, onEntityCreated } from "./ecs";
import * as RenderGroundAnimation from "./systems/render/groundAnimation";
import { Direction, next, prev } from "./utils/direction";
import {
  beltCurvedLeft,
  beltCurvedRight,
  straightBelt,
} from "./utils/assets/beltAnimations";
import { TransportLineSystem } from "./systems/transportLines";
import { showTransportLines } from "./systems/render/debugTransportLines";
import { identity23, Mat23Like } from "@thi.ng/matrices";
import {
  hoveredPosition,
  outlineHoveredTile,
} from "./systems/render/outlineHoveredTile";
import { divN2, Vec2Like } from "@thi.ng/vectors";
import { renderPreview } from "./systems/render/renderPreview";
import { items } from "./items";
import { pressedKeys } from "./keyboard";
import { entityAt } from "./systems/positioning";

const canvas = document.getElementById(`canvas`) as HTMLCanvasElement;
const ctx = canvas.getContext(`2d`)!;

const env: Env = {
  tick: 0,
  ctx,
  mousePosition: [0, 0],
  camera: identity23([]) as Mat23Like,
  screenToPixelRatio: 1,
  player: {
    holding: {
      direction: Direction.Left,
      item: 0,
    },
  },
  items,
  keyboard: pressedKeys(),
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

  env.screenToPixelRatio = wheight / screenHeight;
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

  renderPreview(env);
  outlineHoveredTile(env);

  requestAnimationFrame(main);
};

main();

canvas.addEventListener(`mousemove`, (e) => {
  env.mousePosition = divN2(
    null,
    [e.clientX, e.clientY],
    env.screenToPixelRatio
  ) as Vec2Like;
});

canvas.addEventListener(`mousedown`, (e) => {
  const position = hoveredPosition(env);

  if (entityAt(position) !== null) return;

  const item = env.items[env.player.holding.item];

  const components: Partial<Components> = {
    ...item.onBuild.static,
    position: new Int32Array(position),
  };

  if (item.onBuild.autoInit.direction)
    components.direction = { direction: env.player.holding.direction };

  if (item.onBuild.autoInit.direction) components.transportLine = { id: null };

  ecs.defEntity(components);
});

env.keyboard.emitter.on("r", (e) => {
  const item = env.items[env.player.holding.item];

  if (!item.onBuild.autoInit.direction) return;

  env.player.holding.direction = e.shiftKey
    ? prev(env.player.holding.direction)
    : next(env.player.holding.direction);
});
