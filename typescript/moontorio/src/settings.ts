export const screenHeight = 128;
export const tileSize = 16;
export const sideFromMiddle = 3;
export const transportLineSizes = {
  straight: 16,
  inner: 12,
  outer: 21,
};

export const debugFlags = {
  showTransportLines: false,
  showTransportLinePaths: false,
};

export const halfTile = tileSize / 2;
export const halfTile2 = [halfTile, halfTile] as const;

export const itemOnBelt = 4;
export const beltSpacePerItem = 2;
