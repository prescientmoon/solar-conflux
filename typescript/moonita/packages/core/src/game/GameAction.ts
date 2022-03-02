export const enum GameEvent {
  OnDespawn,
  OnPathfindingGoalReached,
}

export const enum GameAction {
  DespawnEntity,
  ClearSprite,
  HandleBulletSpawner,
  DebugLog,
  ShootWand,
}

// ========== Constants
// Well, the name is deceiving. The max action count is 2^this
export const maxActionCount = 6;
export const maxEventCount = 4; // Same thing here
// a maximum of 64 game actions should be enough for now
export const actionIdMask = (1 << maxActionCount) - 1;
// a maximum of 64 game events should be enough for now
export const eventIdMask = (1 << maxEventCount) - 1;

// ========== Constructors
export function despawnEntity(eid: number) {
  return (eid << maxActionCount) | GameAction.DespawnEntity;
}

export function handleBulletSpawner(eid: number) {
  return (eid << maxActionCount) | GameAction.HandleBulletSpawner;
}

export function shootWand(eid: number) {
  return (eid << maxActionCount) | GameAction.ShootWand;
}

export function clearSprite(eid: number) {
  return (eid << maxActionCount) | GameAction.ClearSprite;
}

export function onDespawn(eid: number) {
  return (eid << maxEventCount) | GameEvent.OnDespawn;
}

export function onPathfindingGoalReached(eid: number) {
  return (eid << maxEventCount) | GameEvent.OnPathfindingGoalReached;
}
