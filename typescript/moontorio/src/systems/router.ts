import { settings } from "../constants";
import type { GameState, TimedItem, RouterConfig } from "../gameState";
import { Entity, ITransform, IUpdate } from "../utils/entity";
import {
  decodeArray,
  decodeNumber,
  decodePair,
  decodeRecord,
  decodeString,
  IToJson,
  Json,
} from "../utils/json";
import { neighbours, neighboursWithSource } from "../utils/machine";
import { Side, Sided, Vec2 } from "../utils/types";
import { BeltItem, IBeltInput, IBeltOutput, tryPushItem } from "./belts";
import { decodeTimedItem, getOptions } from "./world";
import { loadAsset } from "./assets";

const texture = loadAsset("assets/router.svg");

export class Router
  extends Entity
  implements IBeltInput, IBeltOutput, IUpdate, ITransform, IToJson {
  public items: Sided<TimedItem[]> = [[], []];

  private directionClock: Sided<number> = [0, 0];

  public config: RouterConfig;
  public size: Vec2;

  public constructor(
    state: GameState,
    public position: Vec2,
    public item: string
  ) {
    super(state);

    const config = getOptions(state, item, `router`);

    if (config === null)
      throw new Error(`Cannot find router config for item ${item}`);

    this.config = config;
    this.size = [config.size, config.size];
  }

  public beltOutputs() {
    return [...neighbours(this.position, this.size)];
  }

  public pushItem(item: BeltItem, side: Side, from: Vec2) {
    const line = this.items[side];

    // Make sure we don't take more than we are allowed to handle
    if (line.length >= this.config.capacity) return false;

    line.push({
      id: item.id,
      birth: this.world.tick + (item.position * this.config.delay) / 100,
    });

    return true;
  }

  public emptyStartingSpace() {
    return settings.itemOnBeltSize;
  }

  public update() {
    const outputs = [...neighboursWithSource(this.position, this.size)];

    for (let side: Side = 0; side < 2; side++) {
      for (const item of this.items[side]) {
        if (item.birth + this.config.delay > this.world.tick) break;

        const beltItem = {
          position: -settings.itemOnBeltSize,
          id: item.id,
        };

        for (let attempt = 0; attempt < outputs.length; attempt++) {
          const output = outputs[this.directionClock[side]];

          const succesful = tryPushItem(
            this,
            output.neighbour,
            beltItem,
            side,
            output.of
          );

          this.directionClock[side] =
            (this.directionClock[side] + 1) % outputs.length;

          if (succesful) {
            this.items[side].shift();
            break;
          }
        }
      }
    }
  }

  public renderBuilding() {
    this.world.ctx.drawImage(
      texture,
      this.position[0] * settings.tileSize,
      this.position[1] * settings.tileSize,
      settings.tileSize * this.config.size,
      settings.tileSize * this.config.size
    );
  }

  // ========== Json serialization
  public encode() {
    return ({
      items: this.items,
      item: this.item,
      directionClock: this.directionClock,
    } as any) as Json; // god knows why ts doesnt allow me to use this directly
  }

  public static decode(json: Json, state: GameState, position: Vec2) {
    const { items, item, directionClock } = decodeRecord({
      item: decodeString,
      directionClock: decodePair(decodeNumber),
      items: decodePair(decodeArray(decodeTimedItem)),
    })(json);

    const self = new Router(state, position, item);

    self.items = items;
    self.directionClock = directionClock;

    return self;
  }
}
