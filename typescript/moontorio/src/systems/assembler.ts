import { settings } from "../constants";
import type { GameState, AssemblerConfig, Recipe } from "../gameState";
import { Entity, ITransform, IUpdate } from "../utils/entity";
import { IToJson, Json } from "../utils/json";
import { Nullable, Vec2 } from "../utils/types";
import { getOptions } from "./world";
import { loadAsset } from "./assets";
import { IItemInput, IItemOutput, Storage } from "./chest";

const texture = loadAsset("assets/wood_crate.svg");

export class Assembler
  extends Entity
  implements IUpdate, ITransform, IItemInput, IItemOutput, IToJson {
  public size: Vec2;
  public config: AssemblerConfig;

  public input: Storage;
  public output: Storage;

  public insertItem: IItemInput[`insertItem`];
  public takeItems: IItemOutput[`takeItems`];

  public recipe: Nullable<Recipe> = null;

  private isFunctioning = false;
  private startedAt = 0;

  public constructor(
    world: GameState,
    public position: Vec2,
    public item: string
  ) {
    super(world);

    const config = getOptions(world, item, `assembler`);

    if (config === null)
      throw new Error(`Cannot find loader config for assembler ${item}`);

    this.config = config;
    this.size = [this.config.size, this.config.size];

    this.input = new Storage(world, 10, false);
    this.output = new Storage(world, 10, false);

    this.insertItem = this.input.insertItem.bind(this.input);
    this.takeItems = this.output.takeItems.bind(this.output);
  }

  public update() {
    if (!this.recipe) return;

    const assembling =
      this.startedAt + this.recipe.time / this.config.speed <= this.world.tick;

    if (assembling) return;

    if (this.isFunctioning) {
      for (const slot of this.output.items) {
        if (slot === null) return;
        if (slot.amount + this.recipe.outputs[slot.id] > slot.stackSize) return;
      }

      for (const slot of this.output.items) {
        slot!.amount += this.recipe.outputs[slot!.id];
      }

      this.isFunctioning = false;
    } else {
      for (const slot of this.input.items) {
        if (slot === null) return;
        if (slot.amount < this.recipe.inputs[slot.id]) return;
      }

      for (const slot of this.input.items) {
        slot!.amount -= this.recipe.inputs[slot!.id];
      }

      this.startedAt = this.world.tick;
      this.isFunctioning = true;
    }
  }

  public setRecipe(recipe: Recipe) {
    this.recipe = recipe;

    this.input.setSlots(recipe.inputs);
    this.output.setSlots(recipe.outputs);

    return this;
  }

  public renderBuilding() {
    this.world.ctx.drawImage(
      texture,
      this.position[0] * settings.tileSize,
      this.position[1] * settings.tileSize,
      settings.tileSize * this.size[0],
      settings.tileSize * this.size[1]
    );
  }

  // ========== Json serialization
  public encode() {
    return null as any;
  }

  public static decode(json: Json, state: GameState, position: Vec2) {
    return null as any;
  }
}
