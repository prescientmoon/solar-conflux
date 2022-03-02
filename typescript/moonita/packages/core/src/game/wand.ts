import { TextureId } from "./assets";
import type { Adt } from "../Adt";
import type { Vector2 } from "./common/Vector";
import type { CircularBuffer } from "../CircularBuffer";
import { Angle, TAU } from "../math";
import * as V from "./common/Vector";

// Constants
export const enum VanillaProjectileId {
  BubbleSpark,
}

export const enum VanillaCardId {
  BubbleSpark,
  BubbleSparkTrigger,
  BubbleSparkTimer,
  DoubleSpell,
  SpeedUp,
  FormationBehindYourBack,
}

// ========== Types
export interface ProjectileStats {
  damage: number;
  speed: number;
  bounces: number;
  lifetime: [from: number, to: number];
  spread: number;
}

/**
 * A projectile can either do nothing special,
 * cast a spell on collision
 * or cast a spell after a given amount of time
 *
 * ...well, timer projectiles also cast on collision,
 * but that's besides the point
 *
 * The type parameter `T` is there to allow
 * carrying around an arbitrary payload
 * for projectiles that will eventually cast a spell
 */
export type ProjectileKind<T = {}> = Adt<{
  normal: {};
  trigger: {
    payload: T;
  };
  timer: {
    time: number;
    payload: T;
  };
}>;

export interface ProjectileBlueprint {
  stats: ProjectileStats;
  sprite: TextureId;
}

export type CardEffect = Adt<{
  projectile: {
    kind: ProjectileKind;
    blueprint: ProjectileBlueprintId;
  };
  multicast: {
    formation: Array<{
      position: Vector2; // position to shoot from relative to cast position
      direction: Angle; // relative rotation
    }>;
  };
  modifier: {
    stats: ProjectileStats;
  };
}>;

export interface Card {
  name: string;
  sprite: TextureId;
  effects: CardEffect[];
  castDelay: number;
  rechargeDelay: number;
  manaCost: number;
}

export type CardId = number; // id for card definitions
export type WandId = number; // id for wand definitions
export type ProjectileBlueprintId = number; // id for general data about each projectile

/** A reference to a card in the deck of a wand */
export interface CardRef {
  /** Id describing the card */
  id: CardId;

  /** Index in the original deck */
  index: number;
}

export interface WandState {
  // Piles:
  discarded: CircularBuffer<CardRef>; // Cards that have been used
  deck: CircularBuffer<CardRef>; // Cards that are yet to be used
  hand: CircularBuffer<CardRef>; // Cards currently used

  // Other data
  mana: number; // amount of mana available
}

export interface ProjectileCastData {
  position: Vector2;
  direction: Angle;
  blueprint: ProjectileBlueprintId;

  continuation: ProjectileKind<CastState>;
}

export interface CastState {
  stats: ProjectileStats;
  projectiles: Array<ProjectileCastData>;

  /** Spells like formations change where projectiles get launched from / what direction they take */
  accumulatedTransform: {
    position: Vector2;
    direction: Angle;
  };

  /** Flag marked to true after wrapping */
  forceRecharge: boolean;
}

export interface Wand {
  shuffle: boolean;

  castDelay: number;
  rechargeDelay: number;

  manaRecharge: number;
  maxMana: number;

  spread: number;
  capacity: number;

  cards: CardId[];
}

// ========== Helpers
/** Create a stats object with everything zero-ed out */
export function noStats(): ProjectileStats {
  return {
    damage: 0,
    speed: 0,
    bounces: 0,
    lifetime: [0, 0],
    spread: 0,
  };
}

/** Merge two stats objects into one, without mutating anything */
export function mergeStats(
  a: ProjectileStats,
  b: ProjectileStats
): ProjectileStats {
  const into = noStats();

  mergeStatsMut(into, a, b);

  return into;
}

/** Merge two stats objects into one. */
export function mergeStatsMut(
  into: ProjectileStats,
  a: ProjectileStats,
  b: ProjectileStats
): ProjectileStats {
  into.damage = a.damage + b.damage;
  into.speed = a.speed + b.speed;
  into.bounces = a.bounces + b.bounces;
  into.spread = a.spread + b.spread;

  into.lifetime = [
    a.lifetime[0] + b.lifetime[0],
    a.lifetime[1] + b.lifetime[1],
  ]; // kind of like vector addition? Might want to go all in and represent this as a vector one day

  return into;
}

// ========== Example cards
// Example wand
export const exampleWand: Wand = {
  shuffle: false,
  castDelay: 1,
  rechargeDelay: 60,
  manaRecharge: 100,
  maxMana: 400,
  spread: TAU / 36,
  capacity: 0,

  cards: [
    VanillaCardId.SpeedUp,
    VanillaCardId.DoubleSpell,
    VanillaCardId.BubbleSpark,
    VanillaCardId.BubbleSpark,
    VanillaCardId.SpeedUp,
  ],
};

// Example projectile blueprint
export const bubbleSparkProjectile: ProjectileBlueprint = {
  sprite: TextureId.BubbleSpark,
  stats: {
    damage: 5,
    bounces: 3,
    lifetime: [100, 200],
    speed: 3,
    spread: 0,
  },
};

// Example cards
export const bubbleSpark: Card = {
  name: "Bubble spark",
  sprite: TextureId.BubbleSpark,
  castDelay: 1,
  rechargeDelay: 1,
  manaCost: 30,
  effects: [
    {
      type: "projectile",
      kind: {
        type: "normal",
      },
      blueprint: VanillaProjectileId.BubbleSpark,
    },
  ],
};

export const doubleSpell: Card = {
  name: "Double spell",
  sprite: null as any,
  castDelay: 0,
  rechargeDelay: 0,
  manaCost: 0,
  effects: [
    {
      type: "multicast",
      formation: Array(2).fill({
        position: V.origin(),
        direction: 0,
      }),
    },
  ],
};

export const speedUp: Card = {
  name: "Speed up",
  sprite: null as any,
  castDelay: 0,
  rechargeDelay: 0,
  manaCost: 0,
  effects: [
    {
      type: "modifier",
      stats: {
        ...noStats(),
        speed: 5,
      },
    },
  ],
};
