import { Vec2 } from "./types";

/**
 * Enumerates all the neighbours of a nxm machine.
 * The `of` prop of the outputs is the tile the neighbour is next to.
 *
 * @param position The position the machine is placed at
 * @param size The size of the machine.
 */
export const neighboursWithSource = function* (
  position: Vec2,
  size: Vec2
): Generator<{ of: Vec2; neighbour: Vec2 }> {
  for (let i = 0; i < size[0]; i++) {
    yield {
      of: [position[0] + i, position[1]],
      neighbour: [position[0] + i, position[1] - 1],
    };
    yield {
      of: [position[0] + i, position[1] + size[1] - 1],
      neighbour: [position[0] + i, position[1] + size[1]],
    };
  }

  for (let j = 0; j < size[1]; j++) {
    yield {
      of: [position[0], position[1] + j],
      neighbour: [position[0] - 1, position[1] + j],
    };
    yield {
      of: [position[0] + size[0] - 1, position[1] + j],
      neighbour: [position[0] + size[0], position[1] + j],
    };
  }
};

export const neighbours = function* (
  position: Vec2,
  size: Vec2
): Generator<Vec2> {
  for (const { neighbour: output } of neighboursWithSource(position, size)) {
    yield output;
  }
};
