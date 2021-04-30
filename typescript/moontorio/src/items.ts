import { Image, Item, ItemConfig, loadAsset, Machine } from "./gameState";
import { implBeltForLoader } from "./systems/loaders";
import { implBeltForBelt } from "./systems/moveBeltItems";

const notMadeYet: Image = null as any;

const loadItem = (name: string) => loadAsset(`assets/items/${name}.svg`);

const ensureAllAreConfigs = <T extends string>(
  configs: Record<T, ItemConfig>
) => configs;

export const items = ensureAllAreConfigs({
  ironPlate: {
    texture: loadItem("iron_plate"),
    stackSize: 200,
  },
  yellowBelt: {
    texture: notMadeYet,
    stackSize: 200,
    components: {
      beltLike: implBeltForBelt,
    },
  },
  yellowLoader: {
    texture: loadItem("yellow_loader"),
    stackSize: 50,
    components: {
      beltLike: implBeltForLoader,
    },
  },
  yellowUnloder: {
    texture: loadItem("yellow_unloader"),
    stackSize: 50,
  },
  distributor: {
    texture: loadItem("distributor"),
    stackSize: 50,
  },
  woodBox: {
    texture: notMadeYet,
    stackSize: 25,
  },
  ironBox: {
    texture: notMadeYet,
    stackSize: 25,
  },
  woodChest: {
    texture: notMadeYet,
    stackSize: 10,
  },
  woodWarehouse: {
    texture: notMadeYet,
    stackSize: 3,
  },
});

/**
 * Ensures you reference an existing item
 */
export const item = <T extends keyof typeof items>(key: T): Item => key;
