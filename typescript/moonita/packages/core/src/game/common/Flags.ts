export const enum Flag {
  // Visual tools
  DebugShowOriginArrow,
  DebugShowBasePaths,
  DebugShowBounds,
  DebugShowQuadTree,
  DebugShowPathfollowingProjections,
  DebugShowPathfollowingForces,
  DebugShowBoidSeparationForces,
  DebugShowPathfollowingGoals,
  DebugShowBoidSeparationBonds,
  DebugGlobalState,
  DebugWrapping,
  DebugShowSelectedEntityPath,

  // Logging related stuff
  DebugWandExecutionLogs,
  DebugEventLogs,

  // Spawning related stuff
  SpawnDebugBulletEmitter,
  SpawnDebugBoids,
}

export type Flags = Record<Flag, boolean>;

export const defaultFlags: Flags = {
  [Flag.DebugShowOriginArrow]: true,
  [Flag.DebugShowBasePaths]: true,
  [Flag.DebugShowBounds]: true,
  [Flag.DebugShowQuadTree]: false,
  [Flag.DebugShowPathfollowingProjections]: false,
  [Flag.DebugShowPathfollowingForces]: false,
  [Flag.DebugShowPathfollowingGoals]: true,
  [Flag.DebugShowBoidSeparationForces]: false,
  [Flag.DebugShowBoidSeparationBonds]: false,
  [Flag.DebugGlobalState]: true,
  [Flag.DebugWrapping]: true,
  [Flag.DebugShowSelectedEntityPath]: false,

  [Flag.DebugWandExecutionLogs]: true,
  [Flag.DebugEventLogs]: false,

  [Flag.SpawnDebugBulletEmitter]: true,
  [Flag.SpawnDebugBoids]: true,
};
