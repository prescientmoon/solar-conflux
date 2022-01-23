export const enum Flag {
  DebugShowOriginArrow,
  DebugShowBasePaths,
  DebugGlobalState,
  SpawnDebugBulletEmitter,
}

export type Flags = Record<Flag, boolean>;

export const defaultFlags = {
  [Flag.DebugShowOriginArrow]: false,
  [Flag.DebugShowBasePaths]: false,
  [Flag.DebugGlobalState]: true,
  [Flag.SpawnDebugBulletEmitter]: false,
};
