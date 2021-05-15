import { Vec2Like } from "@thi.ng/vectors";
import { components, createGroup, ecs } from "../ecs";
import { addDirection, directions } from "../utils/direction";

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

export const neighbours = (id: number) => {
  const position = components.position.get(id);

  if (position === undefined) return [];

  return directions
    .map((direction) => {
      const neighbour = entityAt(addDirection(position, direction));

      return { direction, id: neighbour };
    })
    .filter((d) => d !== null) as { id: number; direction: number }[];
};
