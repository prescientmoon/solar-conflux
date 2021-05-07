import { debugFlags, settings } from "../constants";
import { GameState, loadAsset } from "../gameState";
import { next, prev } from "../utils/direction";
import { Direction, Nullable, Pair, Side, Vec2 } from "../utils/types";
import { renderTileWithDirection } from "./utils/renderTileWithDirection";
import { add2, dist2, mulN2, sub2 } from "@thi.ng/vectors";
import { reversed } from "../utils/array";
import { BeltCurve, BeltItem, TransportLine } from "../systems/belts";

/**
 * Represents the path items should take through a belt.
 *
 * Each key represents the percentage of the path the point is placed at.
 *
 * Example:
 * ```ts
 * const path = [
 *  [0, [1, 0]],  // Start at (1, 0)
 *  [25, [2, 0]], // Use the first 25% of the time to go to (2, 0)
 *  [100, [2, 2]] // Use the rest of the time to go to (2, 2)
 * ]
 * ```
 */
type BeltPath = Array<[number, Vec2]>;

const halfTile = settings.tileSize / 2;

const straightBeltLeftSidePath: BeltPath = [
  [0, [halfTile - settings.sideFromMiddle, 0]],
  [100, [halfTile - settings.sideFromMiddle, settings.tileSize]],
];

const bentRightInnerSidePath: BeltPath = [
  [0, [0, halfTile + settings.sideFromMiddle]],
  [
    50,
    [halfTile - settings.sideFromMiddle, halfTile + settings.sideFromMiddle],
  ],
  [100, [halfTile - settings.sideFromMiddle, settings.tileSize]],
];

const bentRightOuterSidePath: BeltPath = [
  [0, [0, halfTile - settings.sideFromMiddle]],
  [
    50,
    [halfTile + settings.sideFromMiddle, halfTile - settings.sideFromMiddle],
  ],
  [100, [halfTile + settings.sideFromMiddle, settings.tileSize]],
];

/** Mirros a belt path on the y-axis */
const mirrorBeltPath = (path: BeltPath): BeltPath =>
  path.map(([time, position]) => [
    time,
    [settings.tileSize - position[0], position[1]],
  ]);

const curvedPaths: Pair<BeltPath> = [
  bentRightOuterSidePath,
  bentRightInnerSidePath,
];

const beltPaths: Record<BeltCurve, Pair<BeltPath>> = {
  [BeltCurve.NoCurve]: [
    straightBeltLeftSidePath,
    mirrorBeltPath(straightBeltLeftSidePath),
  ],
  [BeltCurve.Right]: [curvedPaths[1], curvedPaths[0]],
  [BeltCurve.Left]: curvedPaths.map(mirrorBeltPath) as Pair<BeltPath>,
};

const textures = {
  [BeltCurve.NoCurve]: loadAsset("assets/belt_straight.svg"),
  [BeltCurve.Left]: loadAsset("assets/belt_bent_left.svg"),
  [BeltCurve.Right]: loadAsset("assets/belt_bent_right.svg"),
};

/**
 * Objects containing all the belt related data we need for rendering
 */
export type ConveyorBeltLike = {
  length(side: Side): number;
  curve(): BeltCurve;
  transportLine: TransportLine;
  direction: Direction;
  position: Vec2;
};

export const beltRenderer = (state: GameState, belt: ConveyorBeltLike) => {
  let texture = textures[belt.curve()];

  renderTileWithDirection(
    state.ctx,
    belt.direction,
    [
      belt.position[0] * settings.tileSize,
      belt.position[1] * settings.tileSize,
    ],
    settings.tileSize,
    () => {
      state.ctx.drawImage(texture, 0, 0, settings.tileSize, settings.tileSize);
    }
  );
};

const itemRenderOrder = function* (
  direction: Direction,
  curve: BeltCurve,
  items: Array<BeltItem>,
  maxPosition: number
): Generator<BeltItem, any, undefined> {
  if (curve === BeltCurve.NoCurve) {
    if (direction === Direction.Down || direction === Direction.Right) {
      yield* items;
    } else if (direction === Direction.Up || direction === Direction.Left) {
      yield* reversed(items);
    }
  } else {
    const firstHalf = itemRenderOrder(
      curve === BeltCurve.Left ? next(direction) : prev(direction),
      BeltCurve.NoCurve,
      items.filter((i) => i.position < maxPosition / 2),
      maxPosition
    );

    const secondHalf = itemRenderOrder(
      direction,
      BeltCurve.NoCurve,
      items.filter((i) => i.position >= maxPosition / 2),
      maxPosition
    );

    const inRenderDirection =
      direction === Direction.Down || direction === Direction.Right;

    if (inRenderDirection) {
      yield* firstHalf;
      yield* secondHalf;
    } else {
      yield* secondHalf;
      yield* firstHalf;
    }
  }
};

export const beltItemRenderer = (state: GameState, belt: ConveyorBeltLike) => {
  renderTileWithDirection(
    state.ctx,
    belt.direction,
    [
      belt.position[0] * settings.tileSize,
      belt.position[1] * settings.tileSize,
    ],
    settings.tileSize,
    (rotation) => {
      const beltCurve = belt.curve();

      for (let side: Side = 0; side < 2; side++) {
        const beltPath = beltPaths[beltCurve][side];
        const maxLength = belt.length(side);

        for (const item of itemRenderOrder(
          belt.direction,
          beltCurve,
          belt.transportLine.items[side],
          maxLength
        )) {
          let position: Nullable<Vec2> = null;
          const squishedPosition = (100 * item.position) / maxLength; // squish the position between 0 and 100

          for (let index = 0; index < beltPath.length; index++) {
            const current = beltPath[index];
            const next = beltPath[index + 1];

            if (squishedPosition === current[0]) {
              position = current[1];
              break;
            }

            if (squishedPosition >= next[0] && index !== beltPath.length - 2)
              continue;

            const delta = sub2(
              [],
              beltPath[index + 1][1],
              beltPath[index][1]
            ) as Vec2;

            // position = current + delta * (item - current) / (next - current)
            position = add2(
              null,
              mulN2(
                null,
                delta,
                (squishedPosition - current[0]) / (next[0] - current[0])
              ),
              current[1]
            ) as Vec2;

            break;
          }

          if (position === null) throw new Error(`Invalid path ${beltPath}`);

          state.ctx.save();
          state.ctx.translate(...position);
          state.ctx.rotate(-rotation);

          state.ctx.drawImage(
            state.items[item.id].texture,
            -settings.itemOnBeltSize / 2,
            -settings.itemOnBeltSize / 2,
            settings.itemOnBeltSize,
            settings.itemOnBeltSize
          );

          state.ctx.restore();
        }

        if (debugFlags.showBeltItemPaths) {
          state.ctx.strokeStyle = side === Side.Left ? `blue` : `red`;
          state.ctx.beginPath();

          state.ctx.moveTo(...beltPath[0][1]);

          for (let index = 1; index < beltPath.length; index++) {
            state.ctx.lineTo(...beltPath[index][1]);
          }

          state.ctx.stroke();
        }
      }
    }
  );
};
