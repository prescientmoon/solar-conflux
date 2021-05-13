import { mulN22 } from "@thi.ng/matrices";
import { addN2, Vec2Like } from "@thi.ng/vectors";
import { Animation, Env } from "../../ecs";
import { tileSize } from "../../settings";
import { Direction } from "../../utils/direction";
import { rotateAround } from "../../utils/rotateAround";

export const renderAnimation = (
  env: Env,
  animation: Animation,
  tilePosition: Vec2Like,
  direction: Direction
) => {
  const frame = Math.floor(env.tick / animation.speed) % animation.length;

  const position = mulN22([], tilePosition, tileSize);

  env.ctx.save();
  rotateAround(
    env.ctx,
    addN2([], position, tileSize / 2) as Vec2Like,
    (direction * Math.PI) / 2
  );

  env.ctx.drawImage(
    animation.spritesheet,
    frame * tileSize,
    0,
    tileSize,
    tileSize,
    position[0],
    position[1],
    tileSize,
    tileSize
  );

  env.ctx.restore();
};
