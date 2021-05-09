import type { ChestConfig, GameState, Item } from "../gameState";
import { renderSimpleTile } from "../render/simpleTile";
import { Entity, ITransform, IUpdate } from "../utils/entity";
import {
  decodeArray,
  decodeNullable,
  decodeNumber,
  decodeRecord,
  decodeString,
  IToJson,
  Json,
} from "../utils/json";
import { Nullable, Vec2 } from "../utils/types";
import { getOptions, getStackSize } from "./world";

// ========= Interfaces
export interface IItemInput {
  insertItem(item: Item): boolean;
}

export interface IItemOutput {
  // TODO: perhaps change the return type to Array<[item, amount]>
  takeItems(amount: number): Item[];
}

export const hasIItemInput = (e: Entity): e is IItemInput & Entity =>
  typeof (e as Entity & IItemInput).insertItem === "function";
export const hasIItemOutput = (e: Entity): e is IItemOutput & Entity =>
  typeof (e as Entity & IItemOutput).takeItems === "function";

// Reusable bare bones inventory
export class Storage implements IItemInput, IItemOutput, IToJson {
  public items: Nullable<{ stackSize: number; id: Item; amount: number }>[];

  public constructor(public world: GameState, public slots: number) {
    this.items = Array(slots).fill(null);
  }

  public insertItem(item: Item) {
    for (let index = 0; index < this.items.length; index++) {
      const slot = this.items[index];

      if (slot === null) {
        this.items[index] = {
          stackSize: getStackSize(this.world, item),
          amount: 1,
          id: item,
        };

        return true;
      } else if (slot.id !== item) continue;
      else if (slot.amount >= slot.stackSize) continue;

      slot.amount++;
      return true;
    }

    return false;
  }

  public takeItems(amount: number): ReturnType<IItemOutput[`takeItems`]> {
    const result: Item[] = [];

    for (let slotIndex = 0; slotIndex < this.items.length; slotIndex++) {
      const slot = this.items[slotIndex];

      if (slot === null) continue;

      while (amount-- && slot.amount--) {
        result.push(slot.id);
      }

      if (slot.amount === 0) {
        this.items[slotIndex] = null;
      }
    }

    return result;
  }

  // ========== Json serialization
  public encode() {
    return this.items;
  }

  public decode(json: Json) {
    const items = decodeArray(
      decodeNullable(
        decodeRecord({
          stackSize: decodeNumber,
          amount: decodeNumber,
          id: decodeString,
        })
      )
    )(json);

    this.items = items;
  }
}

export class Chest
  extends Entity
  implements ITransform, IUpdate, IItemInput, IItemOutput, IToJson {
  public config: ChestConfig;
  public size: Vec2;
  public storage: Storage;
  public insertItem: IItemInput["insertItem"];
  public takeItems: IItemOutput["takeItems"];

  public constructor(
    state: GameState,
    public position: Vec2,
    public item: string
  ) {
    super(state);

    const config = getOptions(state, item, `chest`);

    if (config === null)
      throw new Error(`Cannot find chest config for item ${item}`);

    this.config = config;
    this.size = [this.config.size, this.config.size];

    this.storage = new Storage(this.world, this.config.slots);
    this.insertItem = this.storage.insertItem.bind(this.storage);
    this.takeItems = this.storage.takeItems.bind(this.storage);
  }

  public update() {}

  public renderBuilding() {
    renderSimpleTile(this, this.item);
  }

  // ========== Json serialization
  public encode() {
    return {
      item: this.item,
      storage: this.storage.encode(),
    };
  }

  public static decode(json: Json, world: GameState, position: Vec2) {
    const { item, storage } = decodeRecord({
      item: decodeString,
      storage: (a) => a,
    })(json);

    const self = new Chest(world, position, item);

    self.storage.decode(storage);

    return self;
  }
}
