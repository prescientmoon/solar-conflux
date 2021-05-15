import { BeltCurve, Components, components, createGroup } from "../ecs";
import { beltAnimations } from "../utils/assets/beltAnimations";
import {
  addDirection,
  Direction,
  directions,
  next,
  opposite,
  relativeTo,
} from "../utils/direction";
import { Side } from "../utils/types";
import { entityAt } from "./positioning";

const group = createGroup(
  [
    components.position,
    components.direction,
    components.beltCurve,
    components.groundAnimation,
  ],
  `Belt curving`
);

const getBeltOutputs = (id: number) => {
  const directionComponent = components.direction.get(id);
  const outputs = components.beltOutputs.get(id);

  if (outputs === undefined) return [];

  const direction = directionComponent ? directionComponent.direction : 0;

  return outputs.ports.map((p) => relativeTo(direction, p));
};

export const updateBeltCurve = (id: number) => {
  const entity = group.getEntity(id);

  // Entity doesn't have one of the required components
  if (entity === undefined) return null;

  const neighbours = directions
    .map((direction) => {
      if (direction === entity.direction.direction) return null;

      const neighbour = entityAt(addDirection(entity.position, direction));

      if (neighbour === null) return null;

      const outputs = getBeltOutputs(neighbour);

      if (!outputs.includes(opposite(direction))) return null;

      return direction;
    })
    .filter((a) => a !== null) as Direction[];

  if (
    neighbours.length !== 1 ||
    neighbours[0] === opposite(entity.direction.direction)
  )
    entity.beltCurve.curve = BeltCurve.NoCurve;
  else {
    entity.beltCurve.curve =
      neighbours[0] === next(entity.direction.direction)
        ? BeltCurve.Right
        : BeltCurve.Left;
  }

  components.groundAnimation.set(id, beltAnimations[entity.beltCurve.curve]);
};

export const setupNewCurves = (id: number) => {
  updateBeltCurve(id);

  const entity = group.getEntity(id);

  // Entity doesn't have one of the required components
  if (entity === undefined) return;

  const machine = entityAt(
    addDirection(entity.position, entity.direction.direction)
  );

  if (machine !== null) updateBeltCurve(machine);
};

export const getBeltCurve = (id: number) =>
  components.beltCurve.get(id)?.curve ?? null;

export const getBeltLength = (id: number, side: Side): number => {
  const curve = getBeltCurve(id);

  // Straight:
  if (curve === null || curve === BeltCurve.NoCurve) return 32; // full size

  // Inner side < outer side
  return (side === Side.Right && curve === BeltCurve.Right) ||
    (side === Side.Left && curve === BeltCurve.Left)
    ? 42
    : 22;
};
