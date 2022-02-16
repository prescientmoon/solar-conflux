export const enum Flag {
  DebugShowOriginArrow,
  DebugShowBasePaths,
  DebugShowBounds,
  DebugShowQuadTree,
  DebugShowPathfollowingProjections,
  DebugShowPathfollowingForces,
  DebugGlobalState,
  DebugWrapping,
  DebugShowSelectedEntityPath,
  SpawnDebugBulletEmitter,
  SpawnDebugBoids,
  TextureCulling,
}

export type Flags = Record<Flag, boolean>;

export const defaultFlags: Flags = {
  [Flag.DebugShowOriginArrow]: true,
  [Flag.DebugShowBasePaths]: false,
  [Flag.DebugShowBounds]: true,
  [Flag.DebugShowQuadTree]: false,
  [Flag.DebugShowPathfollowingProjections]: false,
  [Flag.DebugShowPathfollowingForces]: false,
  [Flag.DebugGlobalState]: true,
  [Flag.DebugWrapping]: true,
  [Flag.DebugShowSelectedEntityPath]: false,
  [Flag.SpawnDebugBulletEmitter]: true,
  [Flag.SpawnDebugBoids]: true,
  [Flag.TextureCulling]: true,
};
