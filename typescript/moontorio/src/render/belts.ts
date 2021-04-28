import { settings } from "../constants";
import { loadAsset, Renderer, Tile } from "../gameState";
import { next, prev } from "../utils/direction";
import { allTiles } from "../utils/traversals";
import { Nullable, Vec2 } from "../utils/types";
import { renderTileWithDirection } from "./utils/renderTileWithDirection";
import { add2, mul2, mulN2, mulS2, sub2 } from "@thi.ng/vectors";

/**
 * All the possible directions a belt can be curved in.
 */
const enum BeltCurve {
  NoCurve,
  Left,
  Right,
}

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

const straightBeltPath: BeltPath = [
  [0, [settings.tileSize / 2, 0]],
  [100, [settings.tileSize / 2, settings.tileSize]],
];

const bentRightPath: BeltPath = [
  [0, [0, settings.tileSize / 2]],
  [50, [settings.tileSize / 2, settings.tileSize / 2]],
  [100, [settings.tileSize / 2, settings.tileSize]],
];

/** Mirros a belt path on the y-axis */
const mirrorBeltPath = (path: BeltPath): BeltPath =>
  path.map(([time, position]) => [
    time,
    [settings.tileSize - position[0], position[1]],
  ]);

const beltPaths: Record<BeltCurve, BeltPath> = {
  [BeltCurve.NoCurve]: straightBeltPath,
  [BeltCurve.Right]: bentRightPath,
  [BeltCurve.Left]: mirrorBeltPath(bentRightPath),
};

const textures = {
  [BeltCurve.NoCurve]: loadAsset("assets/belt_straight.svg"),
  [BeltCurve.Left]: loadAsset("assets/belt_bent_left.svg"),
  [BeltCurve.Right]: loadAsset("assets/belt_bent_right.svg"),
};

/**
 * Check whether (and in what direction) a belt is curved.
 * @param tile The belt to get the curve of.
 */
const getBeltCurve = (tile: Tile): BeltCurve => {
  if (tile.machine.inputs.length === 1) {
    if (next(tile.machine.direction) === tile.machine.inputs[0])
      return BeltCurve.Right;
    if (prev(tile.machine.direction) === tile.machine.inputs[0])
      return BeltCurve.Left;
  }

  return BeltCurve.NoCurve;
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
          const xPos = (settings.tileSize - settings.itemOnBeltSize) / 2;

          const beltCurve = getBeltCurve(tile);
          const beltPath = beltPaths[beltCurve];

          for (const item of tile.machine.items) {
            let position: Nullable<Vec2> = null;

            for (let index = 0; index < beltPath.length; index++) {
              if (item.position === beltPath[index][0]) {
                position = beltPath[index][1];
                break;
              }
              if (item.position >= beltPath[index + 1][0]) continue;

              const current = beltPath[index];
              const next = beltPath[index + 1];

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
                  (item.position - current[0]) / (next[0] - current[0])
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
              state.items[item.item].texture,
              -settings.itemOnBeltSize / 2,
              -settings.itemOnBeltSize / 2,
              settings.itemOnBeltSize,
              settings.itemOnBeltSize
            );

            state.ctx.restore();
          }
        }
      );
    }
  },
};
