import { settings } from "../constants";
import {
  GameState,
  getOptions,
  loadAsset,
  TimedItem,
  RouterConfig,
} from "../gameState";
import { Entity, ITransform, IUpdate } from "../utils/entity";
import { neighbours, neighboursWithSource } from "../utils/machine";
import { Direction, Side, Sided, Vec2 } from "../utils/types";
import { BeltItem, IBeltInput, IBeltOutput, tryPushItem } from "./belts";

const texture = loadAsset("assets/router.svg");

export class Router
  extends Entity
  implements IBeltInput, IBeltOutput, IUpdate, ITransform {
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
      throw new Error(`Cannot find loader config for item ${item}`);

    this.config = config;
    this.size = [config.size, config.size];
  }

  public beltOutputs() {
    return [...neighbours(this.position, [this.config.size, this.config.size])];
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

  public update() {
    const outputs = [...neighboursWithSource(this.position, this.size)];

    for (let side: Side = 0; side < 2; side++) {
      for (const item of this.items[side]) {
        if (item.birth + this.config.delay > this.world.tick) break;

        const beltItem = {
          position: 0,
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
}
