import { Image, Item, ItemConfig, loadAsset, Machine } from "./gameState";
import { TransportLineConfig } from "./systems/belts";

const notMadeYet: Image = null as any;

const loadItem = (name: string) => loadAsset(`assets/items/${name}.svg`);

const ensureAllAreConfigs = <T extends string>(
  configs: Record<T, ItemConfig>
) => configs;

const yellowTransportLine: TransportLineConfig = {
  itemSpacing: 10,
  speed: 1,
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
    texture: loadItem("yellow_loader"),
    stackSize: 50,
    options: {
      type: `loader`,
      ...yellowTransportLine,
    },
  },
  yellowUnloder: {
    texture: loadItem("yellow_unloader"),
    stackSize: 50,
  },
  yellowDistributor: {
    texture: notMadeYet,
    stackSize: 50,
    tileTexture: loadAsset("assets/yellow_distributor.svg"),
  },
  yellowJunction: {
    texture: loadItem("yellow_junction"),
    stackSize: 50,
    options: {
      type: `junction`,
      capacity: 20,
      delay: 30,
    },
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
