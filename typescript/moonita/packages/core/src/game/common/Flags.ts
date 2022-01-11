export const enum Flag {
  DebugShowOriginArrow,
  DebugShowBasePaths,
}

export type Flags = Record<Flag, boolean>;

export const defaultFlags = {
  [Flag.DebugShowOriginArrow]: true,
  [Flag.DebugShowBasePaths]: false,
};
