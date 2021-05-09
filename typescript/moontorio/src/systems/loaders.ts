import { mulN2 } from "@thi.ng/vectors";
import { settings } from "../constants";
import type { GameState } from "../gameState";
import { beltItemRenderer, beltRenderer } from "../render/belts";
import { renderTileWithDirection } from "../render/utils/renderTileWithDirection";
import { addDirection, opposite, relativeTo } from "../utils/direction";
import { Entity, IUpdate } from "../utils/entity";
import {
  decodeDirection,
  decodeDirectional,
  decodeRecord,
  decodeString,
  IToJson,
  Json,
} from "../utils/json";
import { Direction, Directional, Side, Vec2 } from "../utils/types";
import {
  BeltCurve,
  BeltItem,
  emptySpaceTil,
  IBeltInput,
  IBeltOutput,
  TransportLine,
  tryPushItem,
} from "./belts";
import { hasIItemInput, hasIItemOutput } from "./chest";
import { getOptions, machineAt, tileAt } from "./world";
import { loadAsset } from "./assets";

const textures = {
  loaderRoof: loadAsset("assets/yellow_loader_roof.svg"),
};

const loaderLength = 100;

// Only used for rendering, increased to remove a glitch where the items would render over the roof
const visualLoaderStretchFactor = 1.3;

// Helpers for serialization
const encodeLoaderLike = (self: Loader | Unloader): Json => ({
  item: self.item,
  transportLine: self.transportLine.encode(),
  direction: self.direction,
});

const deocdeLoaderLike = (class_: typeof Loader | typeof Unloader) => (
  json: Json,
  world: GameState,
  position: Vec2
) => {
  const { item, transportLine, direction } = decodeRecord({
    item: decodeString,
    direction: decodeDirection,
    transportLine: (a) => a,
  })(json);

  const self = new class_(world, direction, position, item);

  self.transportLine.decode(transportLine);

  return self;
};

export class Loader extends Entity implements IBeltInput, IUpdate, IToJson {
  public transportLine: TransportLine;
  public size: Vec2 = [1, 1];

  public constructor(
    state: GameState,
    public direction: Direction,
    public position: Vec2,
    public item: string
  ) {
    super(state);

    const config = getOptions(state, item, `loader`);

    if (config === null)
      throw new Error(`Cannot find loader config for item ${item}`);

    this.transportLine = new TransportLine(config, (side, item) => {
      const next = tileAt(
        this.world,
        addDirection(this.position, this.direction)
      );

      if (next === null || !hasIItemInput(next.machine)) return false;

      return next.machine.insertItem(item.id);
    });
  }

  //   TODO: allow some form of side loading
  public pushItem(item: BeltItem, side: Side, from: Vec2) {
    const direction = relativeTo(this.position, from);

    // We currently only allow items to come from the input side only
    if (direction !== opposite(this.direction)) return false;

    return this.transportLine.pushItem(item, side, loaderLength);
  }

  public emptyStartingSpace(side: Side) {
    return this.transportLine.emptyStartingSpace(loaderLength, side);
  }

  public update() {
    this.transportLine.update([loaderLength, loaderLength]);
  }

  // Here in order to reuse the belt renderer
  public curve() {
    return BeltCurve.NoCurve;
  }

  // Here in order to reuse the belt renderer
  public length() {
    return loaderLength * visualLoaderStretchFactor;
  }

  public renderGround() {
    beltRenderer(this.world, this);
  }

  public renderItems() {
    beltItemRenderer(this.world, this);
  }

  public renderBuilding() {
    renderTileWithDirection(
      this.world.ctx,
      this.direction,
      mulN2([], this.position, settings.tileSize) as Vec2,
      settings.tileSize,
      () => {
        this.world.ctx.drawImage(textures.loaderRoof, 0, 0);
      }
    );
  }

  // ========== Json serialization
  public encode() {
    return {
      item: this.item,
      transportLine: this.transportLine.encode(),
      direction: this.direction,
    };
  }

  public static decode = deocdeLoaderLike(Loader);
}

// TODO: maybe create an internal belt
export class Unloader extends Entity implements IBeltOutput, IUpdate {
  public transportLine: TransportLine;
  public size: Vec2 = [1, 1];

  public constructor(
    state: GameState,
    public direction: Direction,
    public position: Vec2,
    public item: string
  ) {
    super(state);

    const config = getOptions(state, item, `unloader`);

    if (config === null)
      throw new Error(`Cannot find unloader config for item ${item}`);

    this.transportLine = new TransportLine(config, (side, item, position) => {
      return tryPushItem(
        this,
        addDirection(this.position, this.direction),
        {
          id: item.id,
          position: position - loaderLength,
        },
        side
      );
    });
  }

  public update() {
    for (let side: Side = 0; side < 2; side++) {
      const bound = this.transportLine.emptyStartingSpace(loaderLength, side);

      if (bound < loaderLength * (visualLoaderStretchFactor - 1)) continue;

      const previous = machineAt(
        this.world,
        addDirection(this.position, opposite(this.direction))
      );

      if (previous === null || !hasIItemOutput(previous)) continue;

      const items = previous.takeItems(1);

      if (items.length === 0) continue;

      this.transportLine.pushItem(
        {
          id: items[0],
          position: loaderLength * (visualLoaderStretchFactor - 1),
        },
        side,
        loaderLength
      );
    }

    this.transportLine.update(
      [loaderLength, loaderLength],
      [emptySpaceTil(this, Side.Left), emptySpaceTil(this, Side.Right)]
    );
  }

  // Here in order to reuse the belt renderer
  public curve() {
    return BeltCurve.NoCurve;
  }

  // Here in order to reuse the belt renderer
  public length() {
    return loaderLength;
  }

  public beltOutputs() {
    return [addDirection(this.position, this.direction)];
  }

  public renderGround() {
    beltRenderer(this.world, this);
  }

  public renderItems() {
    beltItemRenderer(this.world, this);
  }

  public renderBuilding() {
    renderTileWithDirection(
      this.world.ctx,
      opposite(this.direction),
      mulN2([], this.position, settings.tileSize) as Vec2,
      settings.tileSize,
      () => {
        this.world.ctx.drawImage(textures.loaderRoof, 0, 0);
      }
    );
  }

  // ========== Json serialization
  public encode() {
    return encodeLoaderLike(this);
  }

  public static decode = deocdeLoaderLike(Unloader);
}
