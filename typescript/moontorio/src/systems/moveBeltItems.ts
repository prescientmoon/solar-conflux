import { GameState, Tile } from "../gameState";
import { chunkSize } from "../map";
import { removeIndex } from "../utils/array";
import { next, opposite } from "../utils/direction";
import { allTiles } from "../utils/traversals";
import { Direction, Pair, Vec2 } from "../utils/types";
import { BeltCurve, getBeltCurve, getBeltLength } from "./beltCurving";

const hashPosition = ([x, y]: Vec2) => (x << 16) | y;

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

export const updateItems = (state: GameState) => {
  const updated = new Set<number>();
  const everyBelt = new Set(state.map.allBelts.map(hashPosition));
  const speed = 1;
  const spacePerItem = 10;

  const update = (pos: Vec2): Tile | null => {
    const num = hashPosition(pos);

    if (updated.has(num)) return null;
    updated.add(num);
    everyBelt.delete(num);

    const tile = tileAt(state, pos);

    if (tile?.machine.type !== "belt") return null;

    const next = tileAt(state, addDirection(pos, tile.machine.direction));

    for (let sideIndex: 0 | 1 = 0; sideIndex < 2; sideIndex++) {
      const side = tile.machine.items[sideIndex];

      // We have to update the items in reverse order in order to prevent pointless collisions
      for (let index = side.length - 1; index >= 0; index--) {
        const item = side[index];

        const sideLength = getBeltLength(sideIndex as 0 | 1, tile);
        const bound =
          index !== side.length - 1
            ? side[index + 1].position - spacePerItem
            : next?.machine.type !== "belt"
            ? sideLength
            : next.machine.items[sideIndex].length === 0
            ? sideLength + getBeltLength(sideIndex as 0 | 1, next)
            : sideLength +
              next.machine.items[sideIndex][0].position -
              spacePerItem;

        const newPosition = Math.min(item.position + speed, bound);

        if (newPosition <= sideLength) item.position = newPosition;
        else {
          side.pop(); // TODO: verify if this is safe
          next!.machine.items[sideIndex].unshift({
            item: item.item,
            position: newPosition - sideLength,
          });
        }
      }
    }

    return tile;
  };

  // TODO: use an actual queue implementation
  const updateQueue = [...state.map.outputBelts];

  while (updateQueue.length || everyBelt.size) {
    const toUpdate = updateQueue.pop();

    if (toUpdate === undefined && everyBelt.size) {
      const first = everyBelt.values().next().value as number;
      console.log(first);

      everyBelt.delete(first);
      const unhashed: Vec2 = [first >> 16, first & ((1 << 16) - 1)];
      console.log(unhashed);

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
