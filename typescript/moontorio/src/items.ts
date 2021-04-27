import { Item, ItemConfig, loadAsset } from "./gameState";

export const items = {
  ironPlate: {
    texture: loadAsset("assets/items/iron_plate.svg"),
  },
  yellowBelt: {
    texture: null as any, // loadAsset("assets/iterms/yellow_belt.svg")
  },
};

/**
 * Ensures you reference an existing item
 */
export const item = <T extends keyof typeof items>(key: T): Item => key;
