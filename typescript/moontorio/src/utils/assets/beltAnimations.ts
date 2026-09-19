import { Animation, BeltCurve, Components } from "../../ecs";
import { loadAsset } from "../assets";

export const straightBelt: Animation = {
  length: 7,
  speed: 4,
  spritesheet: loadAsset(`assets/belts.png`),
  start: 0,
};

export const beltCurvedLeft: Animation = {
  length: straightBelt.length,
  speed: straightBelt.speed,
  spritesheet: loadAsset(`assets/belt_curved_left.png`),
  start: 2,
};

export const beltCurvedRight: Animation = {
  length: straightBelt.length,
  speed: straightBelt.speed,
  spritesheet: loadAsset(`assets/belt_curved_right.png`),
  start: beltCurvedLeft.start,
};

export const beltAnimations: Record<BeltCurve, Animation> = {
  [BeltCurve.NoCurve]: straightBelt,
  [BeltCurve.Left]: beltCurvedLeft,
  [BeltCurve.Right]: beltCurvedRight,
};
