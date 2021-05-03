import type { Belt, Tile } from "../gameState";
import { next, prev } from "../utils/direction";

/**
 * All the possible directions a belt can be curved in.
 */
export const enum BeltCurve {
  NoCurve,
  Left,
  Right,
}
/**
 * Check whether (and in what direction) a belt is curved.
 * @param tile The belt to get the curve of.
 */
export const getBeltCurve = (tile: Belt): BeltCurve => {
  if (tile.machine.inputs.length === 1) {
    if (next(tile.machine.direction) === tile.machine.inputs[0])
      return BeltCurve.Right;
    if (prev(tile.machine.direction) === tile.machine.inputs[0])
      return BeltCurve.Left;
  }

  return BeltCurve.NoCurve;
};

/**
 * Measures a side of a belt.
 * Eg: the outer and inner sides of curved belts have different lengths
 *
 * @param side The side of the belt to measure
 * @param tile The tile to measure
 * @returns How long the particular side is.
 */
export const getBeltLength = (side: 0 | 1, tile: Belt): number => {
  const curve = getBeltCurve(tile);

  // Straight:
  if (curve === BeltCurve.NoCurve) return 100; // full size

  // Inner side < outer side
  return side ? 85 : 115;
};
