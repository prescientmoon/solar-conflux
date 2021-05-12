import { ECS, EVENT_ADDED } from "@thi.ng/ecs";
import { Image } from "./utils/assets";
import { Direction } from "./utils/direction";

export type Components = {
  groundAnimation: {
    spritesheet: Image;
    speed: number;
    length: number;
  };
  position: Int32Array;
  direction: {
    direction: Direction;
  };
  transportLine: {
    id: number | null;
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
};

export interface Env {
  ctx: CanvasRenderingContext2D;
  tick: number;
}

export const onEntityCreated = <T>(
  ecs: ECS<T>,
  listener: (id: number) => void
) => {
  ecs.addListener(EVENT_ADDED, (event) => listener(event.value));
};

type ComponentDef = typeof components[keyof typeof components];

const owned = new Set<ComponentDef>();

export const createGroup = (include: ComponentDef[], name: string) => {
  const toOwn: ComponentDef[] = [];

  for (const component of include) {
    if (owned.has(component)) continue;

    toOwn.push(component);
    owned.add(component);
  }

  return ecs.defGroup(include, toOwn, { id: name });
};
