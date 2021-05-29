import { Env } from "../../ecs";
import { straightBelt } from "../../utils/assets/beltAnimations";
import { Direction } from "../../utils/direction";
import { renderAnimation } from "./animation";
import { hoveredPosition } from "./outlineHoveredTile";

export const renderPreview = (env: Env) => {
  const item = env.items[env.player.holding.item];

  if (item.onBuild === null) return;

  env.ctx.save();
  env.ctx.globalAlpha = 1;

  renderAnimation(
    env,
    item.onBuild.preview,
    hoveredPosition(env),
    item.onBuild.autoInit.direction ? env.player.holding.direction : 0
  );

  env.ctx.restore();
};
