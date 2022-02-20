import { mat3 } from "gl-matrix";
import { all, ECS, types } from "wolf-ecs";
import { QuadTree } from "../QuadTree";
import { TickScheduler } from "../TickScheduler";
import { Texture } from "./assets";
import { AABB } from "./common/AABB";
import { Camera as Camera2d } from "./common/Camera";
import { Flag, Flags } from "./common/Flags";
import { Path } from "./common/Path";
import * as V from "./common/Vector";
import { Map } from "./Map";
import { SpriteRenderer } from "./webgl/SpriteRenderer";

export type ComponentMap = ReturnType<typeof createComponents>;
export type QueryMap = ReturnType<typeof createQueries>;
export type Query = ReturnType<ECS["createQuery"]>;

export const enum LayerId {
  BuildingLayer,
  UnitLayer,
  BulletLayer,
  DebugLayer,
  Unclearable,
  WebglLayer,
  LastLayer,
}

export interface State {
  contexts: Array<CanvasRenderingContext2D>;
  gl: WebGL2RenderingContext;
  projectionMatrix: mat3;
  worldMatrix: mat3;
  webglRenderers: {
    spriteRenderer: SpriteRenderer;
  };
  ecs: ECS;
  tick: number;
  components: ComponentMap;
  queries: QueryMap;
  assets: ReadonlyArray<Texture>;
  textures: ReadonlyArray<WebGLTexture>;
  map: Map;
  camera: Camera2d;
  screenTransform: Camera2d;
  flags: Flags;
  structures: {
    boidQuadTrees: QuadTree[];
  };
  paths: Array<Path>;
  bounds: AABB;

  // Here for debugging
  selectedEntity: SelectedEntity | null;

  tickScheduler: TickScheduler<number>;
}

export interface SelectedEntity {
  id: number;
  isPathFollower: boolean; // whether we should show data related to the path following behavior on this entity
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

export const TransformMatrix = types.any<mat3>();

export const SeekingBehavior = {
  target: Vector2,
};

const PathFollowingBehavior = (flags: Flags) => {
  const result = {
    path: types.uint8,
    debugData: {
      projection: Vector2,
      force: Vector2,
      hasProjection: types.ushort, // boolean
      followedSegment: types.ushort, // the index of the currently followed segment
    },
  };

  if (!flags[Flag.DebugShowPathfollowingProjections]) {
    result.debugData.projection = undefined as any;
  }
  if (!flags[Flag.DebugShowPathfollowingForces]) {
    result.debugData.force = undefined as any;
  }
  if (!flags[Flag.DebugShowSelectedEntityPath]) {
    result.debugData.followedSegment = undefined as any;
  }
  if (
    !flags[Flag.DebugShowSelectedEntityPath] &&
    !flags[Flag.DebugShowPathfollowingForces] &&
    !flags[Flag.DebugShowPathfollowingProjections]
  )
    result.debugData.hasProjection = undefined as any;

  return result;
};

export const ThrusterData = {
  thrusters: types.ushort,
};

// ========== Helpers
export const createComponents = (ecs: ECS, flags: Flags) => {
  const transform = ecs.defineComponent(Transform);
  const transformMatrix = ecs.defineComponent(TransformMatrix);
  const velocity = ecs.defineComponent(Vector2);
  const acceleration = ecs.defineComponent(Vector2);
  const angularVelocity = ecs.defineComponent(types.f32);
  const seekingBehavior = ecs.defineComponent(SeekingBehavior);
  const pathFollowingBehavior = ecs.defineComponent(
    PathFollowingBehavior(flags)
  );
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
  const team = ecs.defineComponent(types.u8);
  const texture = ecs.defineComponent({
    textureId: types.u8,
    width: types.u8,
    height: types.u8,
    layer: types.u8,
  });
  const sprite = ecs.defineComponent({
    textureId: types.u8,
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
    transformMatrix,
    bullet,
    bulletEmitter,
    mortal,
    texture,
    sprite,
    created,
    teamBase,
    angularVelocity,
    seekingBehavior,
    physicsObject,
    thrusters,
    pathFollowingBehavior,
    boidSeparation,
    boidAlignment,
    boidCohesion,
    rotateAfterVelocity,
    speedLimit,
    team,
    layers: Array(LayerId.LastLayer)
      .fill(1)
      .map(() => ecs.defineComponent()),
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
    sprite: ecs.createQuery(
      all<any>(components.sprite, components.transformMatrix)
    ),
    teamBase: ecs.createQuery(
      all<any>(components.teamBase, components.texture)
    ),
    boidSeek: ecs.createQuery(
      all<any>(
        components.seekingBehavior,
        components.physicsObject,
        components.transform,
        components.velocity
      )
    ),
    boidPathFollowing: ecs.createQuery(
      all<any>(
        components.pathFollowingBehavior,
        components.physicsObject,
        components.transform,
        components.velocity
      )
    ),
    boidSeparation: ecs.createQuery(
      all<any>(
        components.velocity,
        components.physicsObject,
        components.boidSeparation,
        components.team,
        components.transform
      )
    ),
    boidAlignment: ecs.createQuery(
      all<any>(
        components.velocity,
        components.physicsObject,
        components.boidAlignment,
        components.team,
        components.transform
      )
    ),
    boidCohesion: ecs.createQuery(
      all<any>(
        components.velocity,
        components.physicsObject,
        components.boidCohesion,
        components.team,
        components.transform
      )
    ),
    rotateAfterVelocity: ecs.createQuery(
      all<any>(components.velocity, components.transform)
    ),
    limitSpeeds: ecs.createQuery(
      all<any>(components.velocity, components.speedLimit)
    ),
    spriteLayers: components.layers.map((layer) =>
      ecs.createQuery(
        all<any>(components.sprite, components.transformMatrix, layer)
      )
    ),
  };
};

// ========== Constants
export const layers = Array(LayerId.LastLayer)
  .fill(1)
  .map((_, i) => i);
