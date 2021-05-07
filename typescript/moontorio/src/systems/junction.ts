import { settings } from "../constants";
import {
  JunctionConfig,
  GameState,
  getOptions,
  loadAsset,
  TimedItem,
} from "../gameState";
import {
  addDirection,
  directions,
  opposite,
  relativeTo,
} from "../utils/direction";
import { Entity, ITransform, IUpdate } from "../utils/entity";
import { neighbours } from "../utils/machine";
import {
  Direction,
  Directional,
  Pair,
  Side,
  Sided,
  Vec2,
} from "../utils/types";
import { BeltItem, IBeltInput, IBeltOutput, tryPushItem } from "./belts";

const texture = loadAsset("assets/junction.svg");

export class Junction
  extends Entity
  implements IBeltInput, IBeltOutput, IUpdate, ITransform {
  public transportLines: Directional<
    Sided<TimedItem[]>
  > = directions.map(() => [[], []]) as any;
  public size: Vec2 = [1, 1];

  public config: JunctionConfig;

  public constructor(
    state: GameState,
    public position: Vec2,
    public item: string
  ) {
    super(state);

    const config = getOptions(state, item, `junction`);

    if (config === null)
      throw new Error(`Cannot find loader config for item ${item}`);

    this.config = config;
  }

  public beltOutputs() {
    return directions.map((d) => addDirection(this.position, d));
  }

  public pushItem(item: BeltItem, side: Side, from: Vec2) {
    const direction = relativeTo(this.position, from);

    if (direction === null) return false;

    const line = this.transportLines[opposite(direction)][side];

    // Make sure we don't take more than we are allowed to handle
    if (line.length >= this.config.capacity) return false;

    line.push({
      id: item.id,
      birth: this.world.tick + (item.position * this.config.delay) / 100,
    });

    return true;
  }

  public emptyStartingSpace(side: Side) {
    return settings.itemOnBeltSize;
  }

  public update() {
    for (let direction: Direction = 0; direction < 4; direction++) {
      for (let side: Side = 0; side < 2; side++) {
        for (const item of this.transportLines[direction][side]) {
          if (item.birth + this.config.delay > this.world.tick) break;

          const succesful = tryPushItem(
            this,
            addDirection(this.position, direction),
            {
              position: -settings.itemOnBeltSize,
              id: item.id,
            },
            side
          );

          if (succesful) this.transportLines[direction][side].shift();
        }
      }
    }
  }

  public renderBuilding() {
    this.world.ctx.drawImage(
      texture,
      this.position[0] * settings.tileSize,
      this.position[1] * settings.tileSize
    );
  }
}
