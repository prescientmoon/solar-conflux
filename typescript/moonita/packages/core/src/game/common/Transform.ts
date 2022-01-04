// ========== Types
export interface Vector2 {
  x: number;
  y: number;
}

export interface Transform {
  rotation: number;
  position: Vector2;
  scale: Vector2;
}

// ========== Constants
export const identityTransform = (): Transform => ({
  rotation: 0,
  position: { x: 0, y: 0 },
  scale: { x: 1, y: 1 },
});
