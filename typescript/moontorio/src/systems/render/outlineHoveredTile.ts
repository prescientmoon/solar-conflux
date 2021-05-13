import { Vec2Like } from "@thi.ng/vectors";
import { Env } from "../../ecs";
import { tileSize } from "../../settings";

export const hoveredPosition = (env: Env) =>
  env.mousePosition.map((p: number) => Math.floor(p / tileSize)) as Vec2Like;

export const outlineHoveredTile = (env: Env) => {
  const position = hoveredPosition(env);

  env.ctx.lineWidth = 2;

  env.ctx.strokeRect(
    position[0] * tileSize,
    position[1] * tileSize,
    tileSize,
    tileSize
  );
};
