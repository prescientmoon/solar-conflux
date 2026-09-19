import {
  add2,
  mulN2,
  rotateAroundPoint2,
  Vec2,
  Vec2Like,
} from "@thi.ng/vectors";
import { BeltCurve, components, Env } from "../../ecs";
import { sideFromMiddle, tileSize, halfTile, halfTile2 } from "../../settings";
import { last, reversed } from "../../utils/array";
import {
  addDirection,
  Direction,
  directionToAngle,
  directionToVector,
  opposite,
  relativeTo,
} from "../../utils/direction";
import { Pair, Side } from "../../utils/types";
import { getBeltCurve, inputDirection } from "../beltCurving";
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

    for (const entity of line.entities) {
      const position = components.position.get(entity)!;

      env.ctx.lineTo(tileSize * position[0], tileSize * position[1]);
    }

    env.ctx.stroke();
  }

  env.ctx.restore();
};

export const showTransportLinePaths = (
  env: Env,
  system: TransportLineSystem
) => {
  env.ctx.save();
  env.ctx.lineWidth = 2;

  let colorIndex = 0;

  for (const line of Object.values(system.transportLines)) {
    env.ctx.strokeStyle = colors[colorIndex];

    colorIndex = (colorIndex + 1) % colors.length;

    const outputDirection =
      components.direction.get(last(line.entities))?.direction ??
      Direction.Right;

    const sideOutputs = [
      halfTile - sideFromMiddle,
      halfTile + sideFromMiddle,
    ].map(
      (y) =>
        add2(
          null,
          mulN2([], line.end, tileSize),
          rotateAroundPoint2(
            null,
            [0, y],
            halfTile2,
            directionToAngle(opposite(outputDirection))
          )
        ) as Vec2Like
    );

    for (let sideIndex: Side = 0; sideIndex < 2; sideIndex++) {
      let position = sideOutputs[sideIndex];

      env.ctx.beginPath();
      env.ctx.moveTo(position[0], position[1]);

      for (const step of reversed(line.sides[sideIndex].path)) {
        position = add2(
          null,
          position,
          directionToVector(opposite(step.direction), step.amount)
        ) as Vec2Like;

        env.ctx.lineTo(position[0], position[1]);
      }

      env.ctx.stroke();
    }
  }

  env.ctx.restore();
};
