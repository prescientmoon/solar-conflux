import * as V from "./Vector";

// ========== Types
export interface Segment {
  from: V.Vector2;
  to: V.Vector2;
}

export function projectPoint(
  segment: Segment,
  point: V.Vector2
): V.Vector2 | null {
  // Let's destructure the segment into [a, b]
  // The following lines "move the coordinate system"
  // so a becomes the origin
  const to = V.sub(segment.to, segment.from);
  const relativePoint = V.sub(point, segment.from);

  const t = V.dotProduct(relativePoint, to) / V.lengthSquared(to);

  if (t < 0 || t > 1) return null;

  // Reuse the vector for performance
  V.scaleMut(to, to, t);
  V.addMut(to, to, segment.from);

  return to;
}
