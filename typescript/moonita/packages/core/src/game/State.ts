import { all, ECS, types } from "wolf-ecs";
import { Texture } from "./assets";

export type ComponentMap = ReturnType<typeof createComponents>;
export type QueryMap = Record<keyof ReturnType<typeof createQueries>, Query>;
export type Query = ReturnType<ECS["createQuery"]>;

export interface State {
  ctx: CanvasRenderingContext2D;
  ecs: ECS;
  tick: number;
  components: ComponentMap;
  queries: QueryMap;
  assets: ReadonlyArray<Texture>;
}

// ========== Runtime type specs
export const Vector2 = {
  x: types.f32,
  y: types.f32,
};

export const Transform = {
  position: Vector2,
  scale: Vector2,
  rotation: types.f32,
};

export const createComponents = (ecs: ECS) => {
  const transform = ecs.defineComponent(Transform);
  const velocity = ecs.defineComponent(Vector2);
  const bulletEmitter = ecs.defineComponent({
    frequency: types.u8,
  });
  const bullet = ecs.defineComponent();
  const mortal = ecs.defineComponent({
    lifetime: types.u16,
  });
  const created = ecs.defineComponent({
    createdAt: types.u32,
  });
  const texture = ecs.defineComponent({
    textureId: types.u8,
    width: types.u8,
    height: types.u8,
  });

  return {
    velocity,
    transform,
    bullet,
    bulletEmitter,
    mortal,
    texture,
    created,
  };
};

export const createQueries = (ecs: ECS, components: ComponentMap) => {
  return {
    kinematics: ecs.createQuery(
      all<any>(components.transform, components.velocity)
    ),
    bullets: ecs.createQuery(
      all<any>(components.transform, components.mortal, components.bullet)
    ),
    bulletEmitters: ecs.createQuery(
      all<any>(
        components.created,
        components.transform,
        components.bulletEmitter
      )
    ),
    mortal: ecs.createQuery(all(components.mortal)),
    textured: ecs.createQuery(
      all<any>(components.texture, components.transform)
    ),
  };
};
