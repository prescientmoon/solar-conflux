import { GameState, getOptions, Item, Machine } from "../gameState";
import {
  addDirection,
  directions,
  next,
  opposite,
  prev,
  relativeTo,
} from "../utils/direction";
import { Direction, Pair, Side, Vec2 } from "../utils/types";
import { Entity, ITransform, IUpdate } from "../utils/entity";
import { machineAt, tileAt } from "./world";
import { eq2, equals2 } from "@thi.ng/vectors";

// ========== Interfaces:
export interface IBeltInput {
  pushItem(item: BeltItem, side: Side, from: Vec2): boolean;
}

export interface IBeltOutput {
  beltOutputs(): Vec2[];
}

export const hasIBeltInput = (e: Entity): e is IBeltInput & Entity =>
  typeof (e as Entity & IBeltInput).pushItem === "function";

export const hasIBeltOutput = (e: Entity): e is IBeltOutput & Entity =>
  typeof (e as Entity & IBeltOutput).beltOutputs === "function";

// ========== Basic types
export type BeltItem = {
  position: number;
  id: Item;
};

/**
 * All the possible directions a belt can be curved in.
 */
export const enum BeltCurve {
  NoCurve,
  Left,
  Right,
}

// ========== Reusable transport belts:
export interface TransportLineConfig {
  speed: number;
  itemSpacing: number;
}

export type OnMoveOut = (
  side: Side,
  item: BeltItem,
  position: number
) => boolean;

export class TransportLine {
  public items: Record<Side, BeltItem[]> = [[], []];

  public constructor(
    public config: TransportLineConfig,
    public onMoveOut: OnMoveOut
  ) {}

  public update(maxLengths: Pair<number>) {
    for (let sideIndex: Side = 0; sideIndex < 2; sideIndex++) {
      const side = this.items[sideIndex];
      const maxLength = maxLengths[sideIndex];

      // We have to update the items in reverse order in order to prevent pointless collisions
      for (let index = side.length - 1; index >= 0; index--) {
        const item = side[index];

        const bound =
          index !== side.length - 1
            ? side[index + 1].position - this.config.itemSpacing
            : Infinity;

        const newPosition = Math.min(item.position + this.config.speed, bound);

        if (newPosition > maxLength) {
          const succesful = this.onMoveOut(sideIndex, item, newPosition);

          if (succesful) side.pop();
          else item.position = maxLength;
        } else {
          item.position = Math.min(newPosition, maxLength);
        }
      }
    }
  }

  public pushItem(item: BeltItem, side: Side, length: number) {
    const sideItems = this.items[side];

    const upperBound =
      sideItems.length === 0
        ? length
        : sideItems[0].position - this.config.itemSpacing;

    const newPosition = Math.min(upperBound, item.position);

    // Cannot handle negative coordinates
    // if (newPosition < 0) return false;

    item.position = newPosition;
    this.items[side].unshift(item);

    return true;
  }
}

// ========== Generic helpers
export const tryPushItem = <T extends Entity & ITransform>(
  self: T,
  nextPosition: Vec2,
  item: BeltItem,
  side: Side,
  from: Vec2 = self.position
) => {
  const next = machineAt(self.world, nextPosition);

  if (next === null || !hasIBeltInput(next)) return false;

  return next.pushItem(item, side, from);
};

// ========== Conveyor belts
export class ConveyorBelt
  extends Entity
  implements IBeltInput, IBeltOutput, ITransform, IUpdate {
  public inputs: Direction[] = [];
  public size: Vec2 = [1, 1];

  public transportLine;

  public constructor(
    state: GameState,
    public direction: Direction,
    public position: Vec2,
    public item: string
  ) {
    super(state);

    const config = getOptions(state, item, `conveyorBelt`);

    if (config === null)
      throw new Error(`Cannot find conveyor belt config for item ${item}`);

    this.transportLine = new TransportLine(config, (side, item, position) => {
      return tryPushItem(
        this,
        addDirection(this.position, this.direction),
        {
          id: item.id,
          position: position - this.length(side),
        },
        side
      );
    });
  }

  /**
   * Measures a side of a belt.
   * Eg: the outer and inner sides of curved belts have different lengths
   *
   * @param side The side of the belt to measure
   * @returns How long the particular side is.
   */
  public length(side: Side): number {
    const curve = this.curve();

    // Straight:
    if (curve === BeltCurve.NoCurve) return 100; // full size

    // Inner side < outer side
    return (side === Side.Right && curve === BeltCurve.Right) ||
      (side === Side.Left && curve === BeltCurve.Left)
      ? 130
      : 70;
  }

  // TODO: handle side loading
  public pushItem(item: BeltItem, side: Side, from: Vec2) {
    const direction = relativeTo(this.position, from);

    // Can only receive items from an adjacent direction
    if (direction === null) return false;

    // This means something is trying to push from the opposite direction
    if (direction === this.direction) return false;

    return this.transportLine.pushItem(item, side, this.length(side));
  }

  public beltOutputs() {
    return [addDirection(this.position, this.direction)];
  }

  public addInput(direction: Direction) {
    if (direction === this.direction) return;

    this.inputs.push(direction);
  }

  public update() {
    this.transportLine.update([
      this.length(Side.Left),
      this.length(Side.Right),
    ]);
  }

  /**
   * Check whether (and in what direction) a belt is curved.
   * @param tile The belt to get the curve of.
   */
  public curve(): BeltCurve {
    if (this.inputs.length === 1) {
      if (next(this.direction) === this.inputs[0]) return BeltCurve.Right;
      if (prev(this.direction) === this.inputs[0]) return BeltCurve.Left;
    }

    return BeltCurve.NoCurve;
  }
}

// ========== Event handlers
export const addBeltLike = (state: GameState, machine: Machine) => {
  if (!hasIBeltOutput(machine)) return null;

  const outputs = machine.beltOutputs();

  for (const neighbourPosition of outputs) {
    const neighbour = machineAt(state, neighbourPosition);

    if (neighbour instanceof ConveyorBelt) {
      const direction = relativeTo(neighbourPosition, machine.position);
      if (direction === null) continue;

      neighbour.addInput(direction);
    }
  }
};

export const addBelt = (state: GameState, machine: Entity) => {
  if (!(machine instanceof ConveyorBelt)) return;

  const inputs = directions
    .map((possibleDirection) => {
      if (possibleDirection === machine.direction) return null;

      const neighbourPosition = addDirection(
        machine.position,
        possibleDirection
      );

      const neighbour = machineAt(state, neighbourPosition);

      if (neighbour === null || !hasIBeltOutput(neighbour)) return null;

      if (!neighbour.beltOutputs().some((p) => equals2(p, machine.position)))
        return null;

      return possibleDirection;
    })
    .filter((a) => a !== null) as Direction[];

  machine.inputs.push(...inputs);
};
