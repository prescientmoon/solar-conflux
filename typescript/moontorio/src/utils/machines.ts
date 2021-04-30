import { Machine, Tile } from "../gameState";

const mkMachineMatcher = <T extends Machine["type"]>(t: T) => (
  a: Tile | null
): a is Tile<T> => a?.machine.type === t;

export const machineIs = <T extends Machine["type"]>(
  t: T,
  a: Tile | null
): a is Tile<T> => a?.machine.type === t;
export const isBelt = mkMachineMatcher("belt");
