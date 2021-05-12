import { ECS } from "@thi.ng/ecs";
import { Image } from "./utils/assets";
import { Direction } from "./utils/direction";

export type Components = {
  groundAnimation: {
    spritesheet: Image;
    speed: number;
    length: number;
  };
  position: Int32Array;
  direction: Direction;
  transportLine: number;
};

export const ecs = new ECS<Components>();

export const components = {
  groundAnimation: ecs.defComponent({
    id: `groundAnimation`,
  })!,
  position: ecs.defComponent({
    id: `position`,
    type: `i32`,
    size: 2,
  })!,
  direction: ecs.defComponent({
    id: `direction`,
  })!,
  transportLine: ecs.defComponent({
    id: `transportLine`,
  }),
};

export interface Env {
  ctx: CanvasRenderingContext2D;
  tick: number;
}
