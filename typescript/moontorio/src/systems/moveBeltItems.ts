import type {
  Belt,
  BeltItem,
  BeltLikePushArgumens,
  GameState,
  MachineComponents,
  Machine,
  Tile,
} from "../gameState";
import { splitPosition, getComponent } from "../gameState";
import { items } from "../items";
import { chunkSize } from "../map";
import { removeIndex } from "../utils/array";
import { opposite } from "../utils/direction";
import { isBelt } from "../utils/machines";
import { Direction, Pair, Side, Vec2 } from "../utils/types";
import { getBeltLength } from "./beltCurving";

// const hashPosition = ([x, y]: Vec2) => (x << 16) | y;

// TODO: make these configurable on a per-item basis
export const speed = 1;
export const spacePerItem = 10;

interface ItemPushEnv<P> {
  state: GameState;
  belt: P;
  side: Side;
  position: Vec2;
  item: BeltItem;
  newPosition: number;
  maxLength: number;
}

export interface BeltLike {
  machine: Pick<Belt["machine"], "items" | "direction">;
}

export const tryPushItem = <T>(
  position: Vec2,
  direction: Direction,
  args: Omit<BeltLikePushArgumens<T>, "belt" | "direction" | "position">
) => {
  const nextPosition = addDirection(position, direction);
  const next = tileAt(args.state, nextPosition);

  if (next === null) return false;

  const beltLike = getComponent(args.state, next.machine.type, "beltLike");

  if (beltLike === null) return false;
  const pushed = beltLike.push({
    ...args,
    belt: next,
    position: nextPosition,
    direction: opposite(direction),
  });

  return pushed;
};

export const pushItemOut = <P extends BeltLike>({
  belt,
  side,
  position,
  item,
  state,
  newPosition,
  maxLength,
}: ItemPushEnv<P>): boolean => {
  if (newPosition <= maxLength) return false;

  return tryPushItem(position, belt.machine.direction, {
    state,
    item: {
      item: item.item,
      position: newPosition - maxLength,
    },
    side,
  });
};

export const implBeltForBelt: MachineComponents<Belt>["beltLike"] = {
  push({ belt, item, side, direction }) {
    if (direction === belt.machine.direction) return false;

    const sideItems = belt.machine.items[side];
    const maxLength = getBeltLength(side, belt);

    const bound =
      sideItems.length === 0 ? maxLength : sideItems[0].position - spacePerItem;

    const newPosition = Math.min(bound, item.position);

    if (newPosition < 0) return false;

    belt.machine.items[side].unshift({
      item: item.item,
      position: newPosition,
    });

    return true;
  },

  outputs(belt) {
    return [belt.machine.direction];
  },
};

const tileAt = (state: GameState, position: Vec2): Tile | null =>
  state.map.chunkMap[Math.floor(position[0] / chunkSize)]?.[
    Math.floor(position[1] / chunkSize)
  ]?.[position[0] % chunkSize]?.[position[1] % chunkSize] ?? null;

const addDirection = (position: Vec2, direction: Direction): Vec2 => {
  if (direction === Direction.Down) return [position[0], position[1] + 1];
  else if (direction === Direction.Up) return [position[0], position[1] - 1];
  else if (direction === Direction.Right) return [position[0] + 1, position[1]];
  else if (direction === Direction.Left) return [position[0] - 1, position[1]];

  throw new Error(`Invalid direction ${direction}`);
};

const directions = [
  Direction.Up,
  Direction.Down,
  Direction.Right,
  Direction.Left,
];

export const addBeltLike = (
  state: GameState,
  machine: Machine,
  position: Vec2
) => {
  // TODO: this is hacky af, dont do this
  const tile: Tile = { subTile: [0, 0], machine };
  const component = getComponent(state, tile.machine.type, "beltLike");

  if (component === null) return;

  const outputs = component.outputs(tile);

  for (const direction of outputs) {
    const neighbourPosition = addDirection(position, direction);
    const neighbour = tileAt(state, neighbourPosition);

    if (!isBelt(neighbour)) continue;
    if (neighbour.machine.direction === opposite(direction)) continue;

    neighbour.machine.inputs.push(opposite(direction));
  }
};

export const addBelt = (state: GameState, machine: Machine, position: Vec2) => {
  if (machine.type !== "belt") return;

  const inputs = directions
    .map((possibleDirection) => {
      if (possibleDirection === machine.direction) return null;

      const neighbourPosition = addDirection(position, possibleDirection);
      const neighbour = tileAt(state, neighbourPosition);

      if (neighbour === null) return null;

      const component = getComponent(state, neighbour.machine.type, "beltLike");

      if (component === null) return null;

      if (!component.outputs(neighbour).includes(opposite(possibleDirection)))
        return null;

      return possibleDirection;
    })
    .filter((a) => a !== null) as Direction[];

  machine.inputs.push(...inputs);
};

interface ItemMoveEnv<P extends BeltLike> {
  tile: P;
  maxLengths: Pair<number>;
  moveOut(side: Side, item: BeltItem, newPosition: number): boolean;
}

export const moveAllItemsOnBelt = <T extends BeltLike>({
  tile,
  moveOut,
  maxLengths,
}: ItemMoveEnv<T>) => {
  for (let sideIndex: Side = 0; sideIndex < 2; sideIndex++) {
    const side = tile.machine.items[sideIndex];
    const maxLength = maxLengths[sideIndex];

    // We have to update the items in reverse order in order to prevent pointless collisions
    for (let index = side.length - 1; index >= 0; index--) {
      const item = side[index];

      const bound =
        index !== side.length - 1
          ? side[index + 1].position - spacePerItem
          : Infinity;

      const newPosition = Math.min(item.position + speed, bound);

      if (newPosition > maxLength) {
        const succesful = moveOut(sideIndex, item, newPosition);

        if (succesful) side.pop();
        else item.position = maxLength;
      } else {
        item.position = Math.min(newPosition, maxLength);
      }
    }
  }
};

export const updteBelt = (state: GameState, tile: Belt, position: Vec2) => {
  moveAllItemsOnBelt({
    tile,
    maxLengths: [
      getBeltLength(Side.Left, tile),
      getBeltLength(Side.Right, tile),
    ],
    moveOut(side, item, newPosition) {
      return pushItemOut({
        state,
        belt: tile,
        newPosition,
        item,
        side,
        position,
        maxLength: getBeltLength(side, tile),
      });
    },
  });
};

// Belt constructor
export const mkBelt = <T extends keyof typeof items>(
  direction: Direction,
  item: T
): Belt["machine"] => ({
  direction,
  inputs: [],
  item,
  items: [[], []],
  type: "belt",
});
