import { settings } from "../constants";
import {
  GameState,
  MachineComponents,
  Junction,
  loadAsset,
} from "../gameState";
import { opposite } from "../utils/direction";
import { Direction, Pair, Side, Vec2 } from "../utils/types";
import { tryPushItem } from "./moveBeltItems";

// TODO: abstract this on a per-item basis
const delay = 30; // ticks
const maxCapacity = 10;

export const implBeltForJunction: MachineComponents<Junction>["beltLike"] = {
  push({ direction, belt, item, side, state }) {
    const transportLine = belt.machine.items[opposite(direction)];

    if (transportLine[side].length >= maxCapacity) return false;

    transportLine[side].push({
      birth: state.tick + (item.position * delay) / 100,
      item: item.item,
    });

    return true;
  },

  outputs() {
    return [Direction.Up, Direction.Left, Direction.Down, Direction.Right];
  },
};

export const updateJunction = (
  state: GameState,
  junction: Junction,
  position: Vec2
) => {
  for (let direction: Direction = 0; direction < 4; direction++) {
    for (let side: Side = 0; side < 2; side++) {
      for (const item of junction.machine.items[direction][side]) {
        if (item.birth + delay > state.tick) break;

        const succesful = tryPushItem(position, direction, {
          item: {
            position: 0,
            item: item.item,
          },
          side,
          state,
        });

        if (succesful) junction.machine.items[direction][side].shift();
      }
    }
  }
};

const texture = loadAsset("assets/junction.svg");

export const renderJunction = (state: GameState, position: Vec2) => {
  state.ctx.drawImage(
    texture,
    position[0] * settings.tileSize,
    position[1] * settings.tileSize
  );
};
