import type { GameState, Item, Machine } from "../gameState";
import {
  addDirection,
  directions,
  next,
  prev,
  relativeTo,
} from "../utils/direction";
import { Direction, Pair, Side, Vec2 } from "../utils/types";
import { Entity, ITransform, IUpdate } from "../utils/entity";
import { getOptions, machineAt } from "./world";
import { equals2 } from "@thi.ng/vectors";
import {
  decodeArray,
  decodeBoolean,
  decodeDirection,
  decodeNumber,
  decodePair,
  Decoder,
  decodeRecord,
  decodeString,
  IToJson,
  Json,
} from "../utils/json";

// ========== Interfaces:
export interface IBeltInput {
  pushItem(item: BeltItem, side: Side, from: Vec2): boolean;
  emptyStartingSpace(side: Side): number;
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

export const decodeBeltItem: Decoder<BeltItem> = decodeRecord({
  position: decodeNumber,
  id: decodeString,
});

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

export class TransportLine implements IToJson {
  public items: Record<Side, BeltItem[]> = [[], []];

  /**
   * If set to true, items might have negative positions (aka be behind the belt visually)
   * Enabled by default for rendering purpouses (smooth items coming out of building animations).
   */
  public allowNegativePositions = true;

  public constructor(
    public config: TransportLineConfig,
    public onMoveOut: OnMoveOut
  ) {}

  public update(maxLengths: Pair<number>, spaceTilNext: Vec2 = [0, 0]) {
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

        if (newPosition >= maxLength) {
          const succesful = this.onMoveOut(sideIndex, item, newPosition);

          if (succesful) {
            side.pop();

            continue;
          }
        }

        item.position = Math.min(
          newPosition,
          maxLengths[sideIndex] + Math.min(spaceTilNext[sideIndex], 0)
        );
      }
    }
  }

  /**
   * Compute how much empty space the belt starts with on a particular side.
   *
   * Examples:
   * - An empty belt will return its length.
   * - A non empty belt will return the space til the first item.
   *
   * @param side The side to get the empty space of.
   */
  public emptyStartingSpace(length: number, side: Side) {
    const sideItems = this.items[side];

    return sideItems.length === 0
      ? length
      : sideItems[0].position - this.config.itemSpacing;
  }

  public pushItem(item: BeltItem, side: Side, length: number) {
    const newPosition = Math.min(
      this.emptyStartingSpace(length, side),
      item.position
    );

    // Handle negative coordinaes
    if (!this.allowNegativePositions && newPosition < 0) return false;

    item.position = newPosition;
    this.items[side].unshift(item);

    return true;
  }

  // ========== Json serialization
  public encode() {
    return {
      items: this.items,
      allowNegativePositions: this.allowNegativePositions,
    };
  }

  public decode(json: Json) {
    const { items, allowNegativePositions } = decodeRecord({
      items: decodePair(decodeArray(decodeBeltItem)),
      allowNegativePositions: decodeBoolean,
    })(json);

    this.items = items;
    this.allowNegativePositions = allowNegativePositions;
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

/**
 * Returns how much empty space there is until the first item on the next conveyor belt.
 */
export const emptySpaceTil = <
  T extends Machine & { direction: Direction; transportLine: TransportLine }
>(
  self: T,
  side: Side
) => {
  const next = machineAt(
    self.world,
    addDirection(self.position, self.direction)
  );

  const spacing = self.transportLine.config.itemSpacing;
  const result =
    next === null || !hasIBeltInput(next)
      ? -spacing
      : next.emptyStartingSpace(side);

  return result;
};

// ========== Conveyor belts
export class ConveyorBelt
  extends Entity
  implements IBeltInput, IBeltOutput, ITransform, IUpdate, IToJson {
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

  public emptyStartingSpace(side: Side) {
    return this.transportLine.emptyStartingSpace(this.length(side), side);
  }

  public beltOutputs() {
    return [addDirection(this.position, this.direction)];
  }

  public addInput(direction: Direction) {
    if (direction === this.direction) return;

    this.inputs.push(direction);
  }

  public update() {
    this.transportLine.update(
      [this.length(Side.Left), this.length(Side.Right)],
      [emptySpaceTil(this, Side.Left), emptySpaceTil(this, Side.Right)]
    );
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

  public next() {
    return machineAt(this.world, addDirection(this.position, this.direction));
  }

  // ========== Json serialization
  public encode() {
    return {
      transportLine: this.transportLine.encode(),
      direction: this.direction,
      position: this.position,
      item: this.item,
      inputs: this.inputs,
    };
  }

  public static decode(json: Json, state: GameState) {
    const { direction, position, item, transportLine, inputs } = decodeRecord({
      direction: decodeDirection,
      position: decodePair(decodeNumber),
      item: decodeString,
      inputs: decodeArray(decodeDirection),
      transportLine: (a) => a,
    })(json);

    const self = new ConveyorBelt(state, direction, position, item);

    self.inputs = inputs;
    self.transportLine.decode(transportLine);

    return self;
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
