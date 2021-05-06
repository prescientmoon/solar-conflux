import { GameState } from "../gameState";
import { Vec2 } from "./types";

export class Entity {
  public constructor(public world: GameState) {}
}

export interface IPosition {
  position: Vec2;
}

// TODO: maybe some sort of prop to disable updating walls and stuff
export interface IUpdate {
  update(): void;
}
