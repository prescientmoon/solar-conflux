import { settings } from "../constants";
import { loadAsset, Renderer, Tile } from "../gameState";
import { next, prev } from "../utils/direction";
import { allTiles } from "../utils/traversals";
import { Nullable, Pair, Vec2 } from "../utils/types";
import { renderTileWithDirection } from "./utils/renderTileWithDirection";
import { add2, mul2, mulN2, mulS2, sub2 } from "@thi.ng/vectors";
import { BeltCurve, getBeltCurve, getBeltLength } from "../systems/beltCurving";

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
  [BeltCurve.Right]: curvedPaths,
  [BeltCurve.Left]: curvedPaths.map(mirrorBeltPath) as Pair<BeltPath>,
};

const textures = {
  [BeltCurve.NoCurve]: loadAsset("assets/belt_straight.svg"),
  [BeltCurve.Left]: loadAsset("assets/belt_bent_left.svg"),
  [BeltCurve.Right]: loadAsset("assets/belt_bent_right.svg"),
};

export const beltRenderer: Renderer = {
  z: 0,
  render: (state) => {
    for (const [tile, position] of allTiles(state)) {
      if (tile?.machine.type !== "belt") continue;

      let texture = textures[getBeltCurve(tile)];

      renderTileWithDirection(
        state.ctx,
        tile.machine.direction,
        [position[0] * settings.tileSize, position[1] * settings.tileSize],
        settings.tileSize,
        () => {
          state.ctx.drawImage(
            texture,
            0,
            0,
            settings.tileSize,
            settings.tileSize
          );
        }
      );
    }
  },
};

export const beltitemRenderer: Renderer = {
  z: 1,
  render: (state) => {
    for (const [tile, position] of allTiles(state)) {
      if (tile?.machine.type !== "belt") continue;

      renderTileWithDirection(
        state.ctx,
        tile.machine.direction,
        [position[0] * settings.tileSize, position[1] * settings.tileSize],
        settings.tileSize,
        (rotation) => {
          const beltCurve = getBeltCurve(tile);

          for (let side: 0 | 1 = 0; side < 2; side++) {
            const beltPath = beltPaths[beltCurve][side];
            for (const item of tile.machine.items[side]) {
              let position: Nullable<Vec2> = null;
              const squishedPosition =
                (100 * item.position) / getBeltLength(side as 0 | 1, tile); // squish the position between 0 and 100

              for (let index = 0; index < beltPath.length; index++) {
                const current = beltPath[index];
                const next = beltPath[index + 1];

                if (squishedPosition === current[0]) {
                  position = current[1];
                  break;
                }

                if (squishedPosition >= next[0]) continue;

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

              if (position === null)
                throw new Error(`Invalid path ${beltPath}`);

              state.ctx.save();
              state.ctx.translate(...position);
              state.ctx.rotate(-rotation);

              state.ctx.drawImage(
                state.items[item.item].texture,
                -settings.itemOnBeltSize / 2,
                -settings.itemOnBeltSize / 2,
                settings.itemOnBeltSize,
                settings.itemOnBeltSize
              );

              state.ctx.restore();
            }
          }
        }
      );
    }
  },
};
