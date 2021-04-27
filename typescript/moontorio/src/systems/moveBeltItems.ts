import { GameState, Tile } from "../gameState";
import { chunkSize } from "../map";
import { removeIndex } from "../utils/array";
import { opposite } from "../utils/direction";
import { allTiles } from "../utils/traversals";
import { Direction, Pair, Vec2 } from "../utils/types";

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
