import { Vec2Like } from "@thi.ng/vectors";
import { Env } from "../../ecs";
import { tileSize } from "../../settings";
import { addDirection } from "../../utils/direction";
import { TransportLineSystem } from "../transportLines";

export const renderItemsOnBelts = (env: Env, system: TransportLineSystem) => {
  env.ctx.strokeStyle = `black`;
  env.ctx.fillStyle = `white`;
  for (const line of Object.values(system.transportLines)) {
    const draw = (p: Vec2Like) => {
      env.ctx.strokeRect(
        p[0] * tileSize - 3,
        p[1] * tileSize - 3,
        tileSize + 6,
        tileSize + 6
      );
      env.ctx.fillRect(
        p[0] * tileSize - 3,
        p[1] * tileSize - 3,
        tileSize + 6,
        tileSize + 6
      );
    };

    let p = line.start;

    draw(p);

    for (const step of line.steps) {
      p = addDirection(p, step);

      draw(p);
    }
  }
};
