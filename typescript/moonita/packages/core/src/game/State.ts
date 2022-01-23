import { all, ECS, types } from "wolf-ecs";
import { Texture } from "./assets";
import { Camera as Camera2d } from "./common/Camera";
import { Flags } from "./common/Flags";
import type { Transform as Transform2d } from "./common/Transform";
import { PolarVector2 } from "./common/Vector";
import { Map } from "./Map";

export type ComponentMap = ReturnType<typeof createComponents>;
export type QueryMap = Record<keyof ReturnType<typeof createQueries>, Query>;
export type Query = ReturnType<ECS["createQuery"]>;

export const enum LayerId {
  BuildingLayer,
  UnitLayer,
  BulletLayer,
  DebugLayer,
  LastLayer,
}

/** A thruster is an engine spaceships can use to move around.
 * Conceptually, this type is equivalent to a Transform
 *  (except the scale is equal on both axis)
 */
export interface Thruster {
  /** Strength of the thruster, measured in newtons */
  strength: number;

  /** The angle at which the thruster is rotated
   * relative to the rotation of the body */
  angle: number;

  /** The position the thruster is placed at
   * relative to the position of the body */
  position: PolarVector2;
}

export interface ThrusterConfiguration {
  // TODO: considering adding a maximum thruster usage budget
  thrusters: Array<Thruster>;
}

export interface State {
  contexts: Array<CanvasRenderingContext2D>;
  ecs: ECS;
  tick: number;
  components: ComponentMap;
  queries: QueryMap;
  assets: ReadonlyArray<Texture>;
  map: Map;
  camera: Camera2d;
  screenTransform: Camera2d;
  flags: Flags;
  thrusterConfigurations: ReadonlyArray<ThrusterConfiguration>;
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

export const SteeringBehavior = {
  target: Vector2,
};

export const ThrusterData = {
  thrusters: types.ushort,
};

// ========== Helpers
export const createComponents = (ecs: ECS) => {
  const transform = ecs.defineComponent(Transform);
  const velocity = ecs.defineComponent(Vector2);
  const acceleration = ecs.defineComponent(Vector2);
  const angularVelocity = ecs.defineComponent(types.f32);
  const seekingBehavior = ecs.defineComponent(SteeringBehavior);
  const thrusters = ecs.defineComponent(ThrusterData);
  const bulletEmitter = ecs.defineComponent({
    frequency: types.u8,
  });
  const speedLimit = ecs.defineComponent(types.f32);
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
    layer: types.u8,
  });
  const teamBase = ecs.defineComponent({
    baseId: types.u8,
  });
  const physicsObject = ecs.defineComponent({
    mass: types.f32,
  });

  const boidSeparation = ecs.defineComponent();
  const boidAlignment = ecs.defineComponent();
  const boidCohesion = ecs.defineComponent();
  const rotateAfterVelocity = ecs.defineComponent();

  return {
    velocity,
    acceleration,
    transform,
    bullet,
    bulletEmitter,
    mortal,
    texture,
    created,
    teamBase,
    angularVelocity,
    seekingBehavior,
    physicsObject,
    thrusters,
    boidSeparation,
    boidAlignment,
    boidCohesion,
    rotateAfterVelocity,
    speedLimit,
  };
};

export const createQueries = (ecs: ECS, components: ComponentMap) => {
  return {
    kinematics: ecs.createQuery(
      all<any>(
        components.transform,
        components.velocity,
        components.acceleration
      )
    ),
    rotating: ecs.createQuery(
      all<any>(components.transform, components.angularVelocity)
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
    teamBase: ecs.createQuery(
      all<any>(components.teamBase, components.texture)
    ),
    seekingBehavior: ecs.createQuery(
      all<any>(
        components.seekingBehavior,
        components.physicsObject,
        components.transform,
        components.velocity,
        components.angularVelocity
      )
    ),
    boidSeparation: ecs.createQuery(
      all<any>(
        components.velocity,
        components.physicsObject,
        components.boidSeparation,
        components.transform
      )
    ),
    boidAlignment: ecs.createQuery(
      all<any>(
        components.velocity,
        components.physicsObject,
        components.boidAlignment,
        components.transform
      )
    ),
    boidCohesion: ecs.createQuery(
      all<any>(
        components.velocity,
        components.physicsObject,
        components.boidCohesion,
        components.transform
      )
    ),
    rotateAfterVelocity: ecs.createQuery(
      all<any>(components.velocity, components.transform)
    ),
    limitSpeeds: ecs.createQuery(
      all<any>(components.velocity, components.speedLimit)
    ),
  };
};

// ========== Constants
export const layers = Array(LayerId.LastLayer)
  .fill(1)
  .map((_, i) => i);
