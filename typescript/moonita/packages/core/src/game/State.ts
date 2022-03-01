import * as PIXI from "pixi.js";
import * as V from "./common/Vector";
import { all, ECS, types } from "wolf-ecs";
import { Flag, Flags } from "./common/Flags";

import type { QuadTree } from "../QuadTree";
import type { TickScheduler } from "../TickScheduler";
import type { AABB } from "./common/AABB";
import type { Camera as Camera2d } from "./common/Camera";
import type { Path } from "./common/Path";
import type { Map } from "./Map";
import type { Transform } from "./common/Transform";
import type { Vector2 } from "./common/Vector";
import type {
  Card,
  CardId,
  ProjectileBlueprint,
  ProjectileBlueprintId,
  Wand,
  WandId,
  WandState,
} from "./wand";

// ========== Types
/** An unique identifier used for referencing an entity */
export type EntityId = number;

/** Type containing all the components registered during a server-side simulation */
export type SimulationComponentMap = ReturnType<
  typeof createSimulationComponents
>;

/** Type containing all the components registered on a full-blown renderable simulation */
export type FullComponentMap = SimulationComponentMap &
  ReturnType<typeof createRenderingComponents>;

export type SimulationQueryMap = ReturnType<typeof createSimulationQueries>;
export type FullQueryMap = SimulationQueryMap &
  ReturnType<typeof createRenderingQueries>;

export type Query = ReturnType<ECS["createQuery"]>;

/** Pre-defined z-indices for pixi objects */
export const enum LayerId {
  BuildingLayer, // Bases, towers, etc
  UnitLayer, // moving units
  BulletLayer, // the name is obvious, why are you reading this
  DebugLayer, // Layer for debug tooling
  LastLayer, // Here as an easy way to get the number of layers
}

/** The game can either run on the server (in headless mode) or on a client (in full blown mode) */
export const enum StateKind {
  Headless,
  Full,
}

/** The state available to the game running on the server side */
export interface SimulationState {
  kind: StateKind;
  ecs: ECS;
  components: SimulationComponentMap;
  queries: SimulationQueryMap;
  map: Map;
  flags: Flags;
  paths: Array<Path>;
  bounds: AABB;
  tickScheduler: TickScheduler<number>;
  structures: {
    boidQuadTrees: QuadTree[];
  };
  tick: number;

  // Wand system related data
  cards: Record<CardId, Card>;
  wands: Record<WandId, Wand>;
  projectileBlueprints: Record<ProjectileBlueprintId, ProjectileBlueprint>;
}

/** The state available to the game running the game on a client */
export interface State extends SimulationState {
  // Overrides:
  kind: StateKind.Full;
  components: FullComponentMap;
  queries: FullQueryMap;

  // New props:
  context: CanvasRenderingContext2D; // debug immediate mode rendering context
  pixiRenderer: PIXI.AbstractRenderer; // main renderer
  pixiStage: PIXI.Container; // root of the main renderer
  pixiTextures: PIXI.Texture[]; // textures for use in the main renderer

  camera: EntityId; // parent of the layer containers
  screenTransform: EntityId; // flips and centers the system of coordinates

  // Here for debugging:
  selectedEntity: SelectedEntity | null;
}

export interface SelectedEntity {
  id: number;
  isPathFollower: boolean; // whether we should show data related to the path following behavior on this entity
}

// ========== Runtime type specs
const typeVector2 = types.custom<Vector2>(() => V.origin());

export const SeekingBehavior = {
  target: typeVector2,
};

const PathFollowingBehavior = (flags: Flags) => {
  const result = {
    path: types.uint8,
    debugData: {
      projection: typeVector2,
      force: typeVector2,
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

const SeparationBehavior = (flags: Flags) => {
  const result = {
    debugData: {
      force: typeVector2,
    },
  };

  if (!flags[Flag.DebugShowBoidSeparationForces]) {
    // @ts-ignore
    delete result.debugData.force;
  }

  return result;
};

// ========== Helpers
export const createSimulationComponents = (ecs: ECS, flags: Flags) => {
  const transform = ecs.defineComponent(types.any<Transform>());
  const velocity = ecs.defineComponent(types.any<Vector2>());
  const acceleration = ecs.defineComponent(typeVector2);
  const angularVelocity = ecs.defineComponent(types.f32);
  const seekingBehavior = ecs.defineComponent(SeekingBehavior);
  const pathFollowingBehavior = ecs.defineComponent(
    PathFollowingBehavior(flags)
  );
  const speedLimit = ecs.defineComponent(types.f32);
  const bullet = ecs.defineComponent();
  const mortal = ecs.defineComponent({
    lifetime: types.u16,
  });
  const created = ecs.defineComponent({
    createdAt: types.u32,
  });
  const team = ecs.defineComponent(types.u8);
  const teamBase = ecs.defineComponent({
    baseId: types.u8,
  });
  const physicsObject = ecs.defineComponent({
    mass: types.f32,
  });

  const boidSeparation = ecs.defineComponent(SeparationBehavior(flags));
  const boidAlignment = ecs.defineComponent();
  const boidCohesion = ecs.defineComponent();
  const rotateAfterVelocity = ecs.defineComponent();

  const wandHolder = ecs.defineComponent({
    wandId: types.uint8, // Wand id
    wandState: types.custom<WandState>(),
  });

  return {
    velocity,
    acceleration,
    transform,
    bullet,
    mortal,
    created,
    teamBase,
    angularVelocity,
    seekingBehavior,
    physicsObject,
    pathFollowingBehavior,
    boidSeparation,
    boidAlignment,
    boidCohesion,
    rotateAfterVelocity,
    speedLimit,
    team,
    wandHolder,
  };
};

export const createRenderingComponents = (ecs: ECS, _: Flags) => {
  const pixiObject = ecs.defineComponent({
    ref: types.any<PIXI.Container>(),
    scaleBySpriteDimenssions: types.uint8, // boolean
  });

  return {
    pixiObject,
  };
};

export const createSimulationQueries = (
  ecs: ECS,
  components: SimulationComponentMap
) => {
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
    mortal: ecs.createQuery(all(components.mortal)),
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
  };
};

export function createRenderingQueries(ecs: ECS, components: FullComponentMap) {
  return {
    pixiObject: ecs.createQuery(
      all<any>(components.pixiObject, components.transform)
    ),
    teamBase: ecs.createQuery(
      all<any>(components.teamBase, components.pixiObject)
    ),
  };
}

// ========== Helpers
export function stateIsComplete(headless: SimulationState): headless is State {
  return headless.kind === StateKind.Full;
}

/** Helper for getting the transform of the camera entity */
export function getCamera(state: State): Camera2d {
  return state.components.transform[state.camera];
}

/** Helper for getting the transform of the screen container entity */
export function getScreenTransform(state: State): Camera2d {
  return state.components.transform[state.screenTransform];
}

// ========== Constants
export const layers = Array(LayerId.LastLayer)
  .fill(1)
  .map((_, i) => i);
