import { mulN2 } from "@thi.ng/vectors";
import { settings } from "../constants";
import { GameState, getOptions, loadAsset } from "../gameState";
import { beltItemRenderer, beltRenderer } from "../render/belts";
import { renderSimpleTile } from "../render/simpleTile";
import { renderTileWithDirection } from "../render/utils/renderTileWithDirection";
import { opposite, relativeTo } from "../utils/direction";
import { Entity, IUpdate } from "../utils/entity";
import { Direction, Directional, Side, Vec2 } from "../utils/types";
import { BeltCurve, BeltItem, IBeltInput, TransportLine } from "./belts";

const textures = {
  loaderRoof: loadAsset("assets/yellow_loader_roof.svg"),
};

const loaderLength = 100;

export class Loader extends Entity implements IBeltInput, IUpdate {
  public transportLine: TransportLine;

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

    this.transportLine = new TransportLine(config, () => {
      console.log(`Item pushed out of loader`);

      return true;
    });
  }

  //   TODO: allow some form of side loading
  public pushItem(item: BeltItem, side: Side, from: Vec2) {
    const direction = relativeTo(this.position, from);

    // We currently only allow items to come from the input side only
    if (direction !== opposite(this.direction)) return false;

    return this.transportLine.pushItem(item, side, loaderLength);
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
    // Only used for rendering, increased to remove a glitch where the items would render over the roof
    return loaderLength * 1.2;
  }

  public renderGround() {
    beltRenderer(this.world, this);
  }

  public renderBuilding() {
    beltItemRenderer(this.world, this);

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
}
