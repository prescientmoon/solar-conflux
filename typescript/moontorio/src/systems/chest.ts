import { ChestConfig, GameState, getOptions } from "../gameState";
import { renderSimpleTile } from "../render/simpleTile";
import { Entity, ITransform, IUpdate } from "../utils/entity";
import { Vec2 } from "../utils/types";

export class Chest extends Entity implements ITransform, IUpdate {
  public config: ChestConfig;
  public size: Vec2;

  public constructor(
    state: GameState,
    public position: Vec2,
    public item: string
  ) {
    super(state);

    const config = getOptions(state, item, `chest`);

    if (config === null)
      throw new Error(`Cannot find loader config for item ${item}`);

    this.config = config;
    this.size = [this.config.size, this.config.size];
  }

  public update() {}

  public renderBuilding() {
    renderSimpleTile(this, this.item);
  }
}
