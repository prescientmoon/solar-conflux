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
  [Flag.DebugShowOriginArrow]: false,
  [Flag.DebugShowBasePaths]: false,
  [Flag.DebugShowBounds]: true,
  [Flag.DebugShowQuadTree]: true,
  [Flag.DebugGlobalState]: true,
  [Flag.DebugWrapping]: true,
  [Flag.SpawnDebugBulletEmitter]: false,
  [Flag.SpawnDebugBoids]: true,
  [Flag.TextureCulling]: false,
};
