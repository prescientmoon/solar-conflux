export const enum Flag {
  DebugShowOriginArrow,
  DebugShowBasePaths,
  DebugShowBounds,
  DebugShowQuadTree,
  DebugShowPathfollowingProjections,
  DebugGlobalState,
  DebugWrapping,
  SpawnDebugBulletEmitter,
  SpawnDebugBoids,
  TextureCulling,
}

export type Flags = Record<Flag, boolean>;

export const defaultFlags: Flags = {
  [Flag.DebugShowOriginArrow]: true,
  [Flag.DebugShowBasePaths]: true,
  [Flag.DebugShowBounds]: true,
  [Flag.DebugShowQuadTree]: false,
  [Flag.DebugShowPathfollowingProjections]: false,
  [Flag.DebugGlobalState]: true,
  [Flag.DebugWrapping]: true,
  [Flag.SpawnDebugBulletEmitter]: false,
  [Flag.SpawnDebugBoids]: true,
  [Flag.TextureCulling]: true,
};
