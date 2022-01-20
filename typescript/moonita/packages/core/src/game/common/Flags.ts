export const enum Flag {
  DebugShowOriginArrow,
  DebugShowBasePaths,
  DebugGlobalState,
}

export type Flags = Record<Flag, boolean>;

export const defaultFlags = {
  [Flag.DebugShowOriginArrow]: true,
  [Flag.DebugShowBasePaths]: true,
  [Flag.DebugGlobalState]: true,
};
