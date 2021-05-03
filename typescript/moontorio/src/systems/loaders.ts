import { mulN2 } from "@thi.ng/vectors";
import { settings } from "../constants";
import {
  Belt,
  GameState,
  MachineComponents,
  loadAsset,
  Loader,
} from "../gameState";
import { beltItemRenderer, beltRenderer } from "../render/belts";
import { renderSimpleTile } from "../render/simpleTile";
import { renderTileWithDirection } from "../render/utils/renderTileWithDirection";
import { Vec2 } from "../utils/types";
import { getBeltCurve } from "./beltCurving";
import { moveAllItemsOnBelt, spacePerItem } from "./moveBeltItems";

const textures = {
  loaderRoof: loadAsset("assets/yellow_loader_roof.svg"),
};

export const implBeltForLoader: MachineComponents<Loader>["beltLike"] = {
  push({ belt, item, side }) {
    const sideItems = belt.machine.items[side];
    const maxLength = 100;

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

  outputs() {
    return [];
  },
};

export const updateLoader = (state: GameState, loader: Loader) => {
  moveAllItemsOnBelt({
    tile: loader,
    maxLengths: [75, 75],
    moveOut(side, item, newPosition) {
      console.log(`Hooray! An item was moved out of a loader:`);
      console.log({ side, item, newPosition });

      return true;
    },
  });
};

export const loaderRenderer = (
  state: GameState,
  loader: Loader,
  position: Vec2
) => {
  // TODO: generalize this so we dont have to do the spread
  const beltLike: Belt = {
    subTile: [0, 0],
    machine: {
      ...loader.machine,
      type: "belt",
      inputs: [],
    },
  };

  beltRenderer(state, beltLike, position);
};

export const loaderItemRenderer = (
  state: GameState,
  loader: Loader,
  position: Vec2
) => {
  // TODO: generalize this so we dont have to do the spread
  const beltLike: Belt = {
    subTile: [0, 0],
    machine: {
      ...loader.machine,
      type: "belt",
      inputs: [],
    },
  };

  beltItemRenderer(state, beltLike, position);

  renderTileWithDirection(
    state.ctx,
    loader.machine.direction,
    mulN2([], position, settings.tileSize) as Vec2,
    settings.tileSize,
    () => {
      state.ctx.drawImage(textures.loaderRoof, 0, 0);
    }
  );
};
