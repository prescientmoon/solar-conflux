import { ComponentID, ECS, EVENT_ADDED, Group, IComponent } from "@thi.ng/ecs";
import { Vec2Like } from "@thi.ng/vectors";
import { Image } from "./utils/assets";
import { Direction } from "./utils/direction";
import { Mat23Like } from "@thi.ng/matrices";
import { Item } from "./items";
import { KeyboardState } from "./keyboard";

export interface Animation {
  spritesheet: Image;
  speed: number;
  length: number;
  start: number;
}

export const enum BeltCurve {
  NoCurve,
  Left,
  Right,
}

export type Components = {
  groundAnimation: Animation;
  previewAnimation: Animation;
  position: Int32Array;
  direction: {
    direction: Direction;
  };
  transportLine: {
    id: number | null;
  };
  beltCurve: {
    curve: BeltCurve;
  };
  beltOutputs: {
    ports: Direction[];
  };
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
  })!,
  beltCurve: ecs.defComponent({
    id: `beltCurve`,
  })!,
  beltOutputs: ecs.defComponent({
    id: `beltOutputs`,
  })!,
};

export interface Env {
  ctx: CanvasRenderingContext2D;
  tick: number;
  mousePosition: Vec2Like;
  keyboard: KeyboardState;
  camera: Mat23Like;
  screenToPixelRatio: number;
  player: {
    holding: {
      direction: Direction;
      item: number;
    };
  };
  items: Record<number, Item>;
}

export const onEntityCreated = <T>(
  ecs: ECS<T>,
  listener: (id: number) => void
) => {
  ecs.addListener(EVENT_ADDED, (event) => listener(event.value));
};

type ComponentDef = typeof components[keyof typeof components];

const owned = new Set<ComponentDef>();

export const createGroup = <T extends ComponentID<Components>>(
  includeRaw: IComponent<T, any, any, any>[],
  name: string
): Group<Components, T> => {
  const include = includeRaw as any as ComponentDef[];
  const toOwn: ComponentDef[] = [];

  for (const component of include) {
    if (owned.has(component)) continue;

    toOwn.push(component);
    owned.add(component);
  }

  return ecs.defGroup(includeRaw, toOwn as any, { id: name });
};
