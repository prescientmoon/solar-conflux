export const enum Flag {
  DebugShowOriginArrow,
}

export type Flags = Record<Flag, boolean>;

export const defaultFlags = {
  [Flag.DebugShowOriginArrow]: false,
};
