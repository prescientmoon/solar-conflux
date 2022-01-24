export const enum Flag {
  DebugShowOriginArrow,
  DebugShowBasePaths,
  DebugGlobalState,
  DebugWrapping,
  SpawnDebugBulletEmitter,
  SpawnDebugBoids,
}

export type Flags = Record<Flag, boolean>;

export const defaultFlags: Flags = {
  [Flag.DebugShowOriginArrow]: false,
  [Flag.DebugShowBasePaths]: false,
  [Flag.DebugGlobalState]: true,
  [Flag.DebugWrapping]: false,
  [Flag.SpawnDebugBulletEmitter]: false,
  [Flag.SpawnDebugBoids]: true,
};
