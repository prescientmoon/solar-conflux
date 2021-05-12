import { Components } from "../../ecs";
import { loadAsset } from "../assets";

export const straightBelt: Components["groundAnimation"] = {
  length: 7,
  speed: 4,
  spritesheet: loadAsset(`assets/belts.png`),
};

export const beltCurvedLeft: Components["groundAnimation"] = {
  length: straightBelt.length,
  speed: straightBelt.speed,
  spritesheet: loadAsset(`assets/belt_curved_left.png`),
};

export const beltCurvedRight: Components["groundAnimation"] = {
  length: straightBelt.length,
  speed: straightBelt.speed,
  spritesheet: loadAsset(`assets/belt_curved_right.png`),
};
