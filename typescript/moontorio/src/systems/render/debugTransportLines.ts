import { Env } from "../../ecs";
import { tileSize } from "../../settings";
import { addDirection } from "../../utils/direction";
import { TransportLineSystem } from "../transportLines";

export const showTransportLines = (env: Env, system: TransportLineSystem) => {
  env.ctx.save();
  env.ctx.translate(tileSize / 2, tileSize / 2);
  env.ctx.strokeStyle = `red`;
  env.ctx.lineWidth = 2;

  for (const line of Object.values(system.transportLines)) {
    env.ctx.beginPath();
    env.ctx.moveTo(tileSize * line.start[0], tileSize * line.start[1]);

    let position = line.start;

    for (const step of line.steps) {
      position = addDirection(position, step);

      env.ctx.lineTo(tileSize * position[0], tileSize * position[1]);
    }

    env.ctx.stroke();
  }

  env.ctx.restore();
};
