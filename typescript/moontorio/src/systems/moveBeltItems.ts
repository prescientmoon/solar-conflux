import { GameState, Tile } from "../gameState";
import { chunkSize } from "../map";
import { removeIndex } from "../utils/array";
import { next, opposite } from "../utils/direction";
import { allTiles } from "../utils/traversals";
import { Direction, Pair, Vec2 } from "../utils/types";

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
      items: [],
      inputs,
    },
  };

  const nextTile = tileAt(state, addDirection(position, direction));

  if (nextTile?.machine.type === "belt") {
    nextTile.machine.inputs.push(opposite(direction));
  }

  if (nextTile === null) state.map.outputBelts.push(position);
};

export const updateItems = (state: GameState) => {
  const updated = new Set<number>();
  const speed = 1;
  const spacePerItem = 10;

  const update = (pos: Vec2): Tile | null => {
    const num = hashPosition(pos);

    if (updated.has(num)) return null;
    updated.add(num);

    const tile = tileAt(state, pos);

    if (tile?.machine.type !== "belt") return null;

    const next = tileAt(state, addDirection(pos, tile.machine.direction));

    // We have to update the items in reverse order in order to prevent pointless collisions
    for (let index = tile.machine.items.length - 1; index >= 0; index--) {
      const item = tile.machine.items[index];

      const bound =
        index !== tile.machine.items.length - 1
          ? tile.machine.items[index + 1].position - spacePerItem
          : next?.machine.type !== "belt"
          ? 100
          : next.machine.items.length === 0
          ? 200
          : 100 + next.machine.items[0].position - spacePerItem;

      const newPosition = Math.min(item.position + speed, bound);

      if (newPosition <= 100) item.position = newPosition;
      else {
        tile.machine.items.pop(); // TODO: verify if this is safe
        next!.machine.items.unshift({
          item: item.item,
          position: newPosition - 100,
        });
      }
    }

    return tile;
  };

  // TODO: use an actual queue implementation
  const updateQueue = [...state.map.outputBelts];

  while (updateQueue.length) {
    const toUpdate = updateQueue.pop()!;

    const updated = update(toUpdate);

    if (updated === null) continue;

    updateQueue.unshift(
      ...updated.machine.inputs.map((d) => addDirection(toUpdate, d))
    );
  }
};
