import { settings } from "../constants";
import {
  BeltItem,
  BeltLikePushArgumens,
  GameState,
  Item,
  loadAsset,
  Tile,
  TimedItem,
} from "../gameState";
import { directions, opposite } from "./direction";
import { Direction, Directional, Pair, Side, Vec2 } from "./types";

export class Entity {
  public constructor(protected state: GameState) {}
}

interface IBeltInput {
  pushItem(item: BeltItem, side: Side, direction: Direction): boolean;
}

interface IBeltOutput {
  outputs(): Direction[];
}

const isIBeltInput = (e: Entity): e is IBeltInput & Entity =>
  typeof (e as any).pushItem === "function";

class TransportLine implements IBeltInput {
  public constructor(private onFinish: (i: BeltItem) => boolean) {}

  public pushItem() {
    // Push item
    return true;
  }

  public update() {
    // Move items

    // Can call this
    this.onFinish(null as any);
  }
}

class ConveyorBelt extends Entity implements IBeltInput, IBeltOutput {
  private trnsportLine = new TransportLine((i) => {
    // Pass item to the next thing
    return true;
  });

  public pushItem = this.trnsportLine.pushItem;

  public outputs() {
    // Returns the direction oriented in
    return [];
  }

  public update() {
    this.trnsportLine.update();
  }
}

class Loader extends Entity implements IBeltInput {
  private transportLine = new TransportLine((i) => {
    // Put item inside the next thing
    return true;
  });

  public pushItem = this.transportLine.pushItem;

  public update() {
    this.transportLine.update();
  }
}

const maxCapacity = 30;
const delay = 30;
const next = (state: GameState, position: Vec2, direction: Direction) => {
  return (null as any) as Entity | null;
};

class Junction extends Entity implements IBeltInput, IBeltOutput {
  static texture = loadAsset("assets/junction.svg");

  private transportLines: Directional<Pair<TimedItem[]>> = [
    [[], []],
    [[], []],
    [[], []],
    [[], []],
  ];

  public constructor(state: GameState, public position: Vec2) {
    super(state);
  }

  public outputs() {
    return directions;
  }

  public pushItem(item: BeltItem, side: Side, direction: Direction): boolean {
    const transportLine = this.transportLines[opposite(direction)];

    if (transportLine[side].length >= maxCapacity) return false;

    transportLine[side].push({
      birth: this.state.tick + (item.position * delay) / 100,
      item: item.item,
    });

    return true;
  }

  public update() {
    for (let direction: Direction = 0; direction < 4; direction++) {
      for (let side: Side = 0; side < 2; side++) {
        for (const item of this.transportLines[direction][side]) {
          if (item.birth + delay > this.state.tick) break;

          const nextTile = next(this.state, this.position, direction);

          if (nextTile === null || !isIBeltInput(nextTile)) continue;

          const succesful = nextTile.pushItem(
            {
              position: 0,
              item: item.item,
            },
            side,
            direction
          );

          if (succesful) this.transportLines[direction][side].shift();
        }
      }
    }
  }

  public render() {
    this.state.ctx.drawImage(
      Junction.texture,
      this.position[0] * settings.tileSize,
      this.position[1] * settings.tileSize
    );
  }
}
