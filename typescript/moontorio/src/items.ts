import type { Animation, Components } from "./ecs";
import { Image, loadAsset } from "./utils/assets";
import { straightBelt } from "./utils/assets/beltAnimations";
import { Direction } from "./utils/direction";

export type OnBuildComponent = "transportLine" | "direction" | "beltCurving";
type StaticComponentNames = "groundAnimation" | "beltOutputs";
export type StaticComponents = { [K in StaticComponentNames]: Components[K] };

export interface Item {
  icon: Image;
  onBuild: {
    autoInit: Partial<Record<OnBuildComponent, boolean>>;
    static: Partial<StaticComponents>;
    preview: Animation;
    straight?: boolean;
  } | null;
  name: string;
}

const ensureItems = <T extends Record<number, Item>>(t: T): T => t;
const notDrawn: Image = null as any;

export const items = ensureItems({
  [0]: {
    icon: notDrawn,
    name: `Yellow belt`,
    onBuild: {
      autoInit: {
        transportLine: true,
        direction: true,
        beltCurving: true,
      },
      static: {
        groundAnimation: straightBelt,
        beltOutputs: {
          ports: [Direction.Right],
        },
      },
      preview: straightBelt,
      straight: true,
    },
  },
  [1]: {
    icon: loadAsset(`assets/iron_plate.png`),
    name: `Iron plate`,
    onBuild: null,
  },
});
