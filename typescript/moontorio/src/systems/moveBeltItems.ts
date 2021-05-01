import {
  Belt,
  BeltItem,
  GameState,
  getComponent,
  Item,
  ItemComponents,
  Tile,
} from "../gameState";
import { chunkSize } from "../map";
import { removeIndex } from "../utils/array";
import { opposite } from "../utils/direction";
import { isBelt } from "../utils/machines";
import { Direction, Pair, Side, Vec2 } from "../utils/types";
import { getBeltLength } from "./beltCurving";

const hashPosition = ([x, y]: Vec2) => (x << 16) | y;

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

export const pushItemOut = <P extends BeltLike>({
  belt,
  side,
  position,
  item,
  state,
  newPosition,
  maxLength,
}: ItemPushEnv<P>): boolean => {
  const nextPosition = addDirection(position, belt.machine.direction);

  if (newPosition < 0) return false;

  const next = tileAt(state, nextPosition);

  if (next !== null && newPosition > maxLength) {
    const beltLike = getComponent(state, next.machine.item, "beltLike");

    if (beltLike !== null) {
      const pushed = beltLike.push({
        state,
        belt: next,
        item: {
          item: item.item,
          position: newPosition - maxLength,
        },
        position: nextPosition,
        side,
      });

      return pushed;
    }
  }

  return false;
};

export const implBeltForBelt: ItemComponents<Belt>["beltLike"] = {
  push({ belt, item, side }) {
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
};

const tileAt = (state: GameState, position: Vec2): Tile | null =>
  state.map.chunkMap[Math.floor(position[0] / chunkSize)]?.[
    Math.floor(position[1] / chunkSize)
  ]?.[position[0] % chunkSize]?.[position[1] % chunkSize] ?? null;

const splitPosition = (position: Vec2): Pair<Vec2> => [
  [Math.floor(position[0] / chunkSize), Math.floor(position[1] / chunkSize)],
  [position[0] % chunkSize, position[1] % chunkSize],
];

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

// export const addDistributor

export const addBelt = (
  state: GameState,
  position: Vec2,
  direction: Direction,
  item: string
) => {
  const [chunkPosition, localPosition] = splitPosition(position);

  const inputs = directions
    .map((possibleDirection) => {
      const neighbourPosition = addDirection(position, possibleDirection);
      const neighbour = tileAt(state, neighbourPosition);

      if (neighbour?.machine.type !== "belt") return null;
      if (neighbour.machine.direction !== opposite(possibleDirection))
        return null;

      return possibleDirection;
    })
    .filter((a) => a !== null) as Direction[];

  for (const input of inputs) {
    const inputPosition = addDirection(position, input);
    let index = state.map.outputBelts.findIndex(
      (output) =>
        output[0] === inputPosition[0] && output[1] === inputPosition[1]
    );
    if (index !== -1) {
      removeIndex(state.map.outputBelts, index);
    }
  }

  state.map.chunkMap[chunkPosition[0]][chunkPosition[1]]![localPosition[0]][
    localPosition[1]
  ] = {
    subTile: [0, 0],
    machine: {
      type: "belt",
      direction,
      item,
      items: [[], []],
      inputs,
    },
  };

  const nextTile = tileAt(state, addDirection(position, direction));

  if (nextTile?.machine.type === "belt") {
    nextTile.machine.inputs.push(opposite(direction));
  }

  if (nextTile === null) state.map.outputBelts.push(position);

  state.map.allBelts.push(position);
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

export const updateAllItemsOnBelts = (state: GameState) => {
  const updated = new Set<number>();
  const everyBelt = new Set(state.map.allBelts.map(hashPosition));

  const update = (pos: Vec2): Belt | null => {
    const num = hashPosition(pos);

    if (updated.has(num)) return null;
    updated.add(num);
    everyBelt.delete(num);

    const tile = tileAt(state, pos);

    if (!isBelt(tile)) return null;

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
          position: pos,
          maxLength: getBeltLength(side, tile),
        });
      },
    });

    return tile;
  };

  // TODO: use an actual queue implementation
  const updateQueue = [...state.map.outputBelts];

  while (updateQueue.length || everyBelt.size) {
    const toUpdate = updateQueue.pop();

    if (toUpdate === undefined && everyBelt.size) {
      const first = everyBelt.values().next().value as number;

      everyBelt.delete(first);
      const unhashed: Vec2 = [first >> 16, first & ((1 << 16) - 1)];

      updateQueue.push(unhashed);
      continue;
    } else if (toUpdate === undefined) break;

    const updated = update(toUpdate);

    if (updated === null) continue;

    updateQueue.unshift(
      ...updated.machine.inputs.map((d) => addDirection(toUpdate, d))
    );
  }
};
