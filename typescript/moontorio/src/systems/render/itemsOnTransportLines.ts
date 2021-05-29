import { add2, mulN2, rotateAroundPoint2, Vec2Like } from "@thi.ng/vectors";
import { components, Env } from "../../ecs";
import {
  beltSpacePerItem,
  halfTile,
  halfTile2,
  itemOnBelt,
  sideFromMiddle,
  tileSize,
} from "../../settings";
import { last, reversed } from "../../utils/array";
import {
  Direction,
  directionToAngle,
  directionToVector,
  opposite,
} from "../../utils/direction";
import { Side } from "../../utils/types";
import {
  TransportLineSystem,
  TransportLinePathSegment,
} from "../transportLines";

export const renderItemsOnTrasnportLines = (
  env: Env,
  system: TransportLineSystem
) => {
  for (const line of system) {
    const outputDirection =
      components.direction.get(last(line.entities))?.direction ??
      Direction.Right;

    const sideOutputs = [
      halfTile - sideFromMiddle,
      halfTile + sideFromMiddle,
    ].map((y) =>
      add2(
        null,
        mulN2([], line.end, tileSize),
        rotateAroundPoint2(
          null,
          [0, y],
          halfTile2,
          directionToAngle(opposite(outputDirection))
        )
      )
    );

    for (let sideIndex: Side = 0; sideIndex < 2; sideIndex++) {
      const side = line.sides[sideIndex];

      let distance = 0;
      let segments = reversed(side.path);
      let currentSegment = segments.next().value as TransportLinePathSegment;
      let position = sideOutputs[sideIndex];

      for (const item of side.items) {
        distance += item.gap + beltSpacePerItem;

        while (distance > currentSegment.amount) {
          const next = segments.next();

          if (next.done) {
            console.log({ distance, currentSegment, next, side });

            throw new Error(`Path is too small for items on top of it`);
          }

          add2(
            null,
            position,
            directionToVector(
              opposite(currentSegment.direction),
              currentSegment.amount
            )
          );

          distance -= currentSegment.amount;
          currentSegment = next.value;
        }

        const itemPosition = add2(
          [],
          position,
          directionToVector(opposite(currentSegment.direction), distance)
        );

        env.ctx.drawImage(
          env.items[item.item].icon,
          itemPosition[0] - itemOnBelt / 2,
          itemPosition[1] - itemOnBelt / 2,
          itemOnBelt,
          itemOnBelt
        );
      }
    }
  }
};
