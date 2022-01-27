export const enum Flag {
  DebugShowOriginArrow,
  DebugShowBasePaths,
  DebugShowBounds,
  DebugShowQuadTree,
  DebugGlobalState,
  DebugWrapping,
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
  [Flag.DebugGlobalState]: true,
  [Flag.DebugWrapping]: true,
  [Flag.SpawnDebugBulletEmitter]: true,
  [Flag.SpawnDebugBoids]: true,
  [Flag.TextureCulling]: true,
};
