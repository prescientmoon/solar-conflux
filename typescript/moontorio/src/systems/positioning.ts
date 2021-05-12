import { Vec2Like } from "@thi.ng/vectors";
import { components, createGroup, ecs } from "../ecs";

const group = createGroup([components.position], `Entity positioning`);

// WARNING: inefficient as fuck
export const entityAt = (position: Vec2Like): number | null => {
  for (const entity of group.values()) {
    if (
      entity.position[0] === position[0] &&
      entity.position[1] === position[1]
    )
      return entity.id;
  }

  return null;
};
