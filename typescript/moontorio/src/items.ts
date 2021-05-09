import type { Image, Item, ItemConfig } from "./gameState";
import type { TransportLineConfig } from "./systems/belts";
import { loadAsset } from "./systems/assets";

const notMadeYet: Image = null as any;

const loadItem = (name: string) => loadAsset(`assets/items/${name}.svg`);
// const loadAsset = (name: string) => notMadeYet;
// const loadItem = (name: string) => notMadeYet;

const ensureAllAreConfigs = <T extends string>(
  configs: Record<T, ItemConfig>
) => configs;

const yellowTransportLine: TransportLineConfig = {
  itemSpacing: 10,
  speed: 1,
};

const chestSizes = {
  wood: 10,
};

export const items = ensureAllAreConfigs({
  ironPlate: {
    texture: loadItem("iron_plate"),
    stackSize: 200,
  },
  yellowBelt: {
    texture: notMadeYet,
    stackSize: 200,
    options: {
      type: `conveyorBelt`,
      ...yellowTransportLine,
    },
  },
  yellowLoader: {
    texture: notMadeYet,
    stackSize: 50,
    options: {
      type: `loader`,
      ...yellowTransportLine,
    },
  },
  yellowUnloder: {
    texture: notMadeYet,
    stackSize: 50,
    options: {
      type: `unloader`,
      ...yellowTransportLine,
    },
  },
  junction: {
    texture: notMadeYet,
    stackSize: 50,
    options: {
      type: `junction`,
      capacity: 20,
      delay: 30,
    },
  },
  router: {
    texture: notMadeYet,
    stackSize: 50,
    options: {
      type: `router`,
      capacity: 20,
      delay: 60,
      size: 1,
    },
  },
  distributor: {
    texture: notMadeYet,
    stackSize: 10,
    options: {
      type: `router`,
      capacity: 80,
      delay: 120,
      size: 2,
    },
  },
  woodBox: {
    texture: notMadeYet,
    stackSize: 25,
    tileTexture: loadAsset("assets/wood_crate.svg"),
    options: {
      type: `chest`,
      size: 1,
      slots: chestSizes.wood,
    },
  },
  woodChest: {
    texture: notMadeYet,
    stackSize: 10,
    tileTexture: loadAsset("assets/wood_crate.svg"),
    options: {
      type: `chest`,
      size: 2,
      slots: 4 * chestSizes.wood,
    },
  },
  woodWarehouse: {
    texture: notMadeYet,
    stackSize: 3,
    tileTexture: loadAsset("assets/wood_crate.svg"),
    options: {
      type: `chest`,
      size: 3,
      slots: 9 * chestSizes.wood,
    },
  },
});

/**
 * Ensures you reference an existing item
 */
export const item = <T extends keyof typeof items>(key: T): Item => key;
