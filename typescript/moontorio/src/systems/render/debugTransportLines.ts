import { Env } from "../../ecs";
import { tileSize } from "../../settings";
import { addDirection } from "../../utils/direction";
import { TransportLineSystem } from "../transportLines";

const colors = [
  `blue`,
  `red`,
  `yellow`,
  `green`,
  `white`,
  `purple`,
  `orange`,
  `pink`,
  `gray`,
];

export const showTransportLines = (env: Env, system: TransportLineSystem) => {
  env.ctx.save();
  env.ctx.translate(tileSize / 2, tileSize / 2);
  env.ctx.lineWidth = 2;

  let colorIndex = 0;

  for (const line of Object.values(system.transportLines)) {
    env.ctx.strokeStyle = colors[colorIndex];

    colorIndex = (colorIndex + 1) % colors.length;

    env.ctx.beginPath();
    env.ctx.moveTo(tileSize * line.start[0], tileSize * line.start[1]);
    env.ctx.strokeRect(
      tileSize * line.start[0],
      tileSize * line.start[1],
      1,
      1
    );

    let position = line.start;

    for (const step of line.steps) {
      position = addDirection(position, step);

      env.ctx.lineTo(tileSize * position[0], tileSize * position[1]);
    }

    env.ctx.stroke();
  }

  env.ctx.restore();
};
