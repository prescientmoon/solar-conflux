import { eq2, equals2, Vec2, Vec2Like } from "@thi.ng/vectors";
import { components, createGroup } from "../ecs";
import {
  addDirection,
  Direction,
  directions,
  opposite,
} from "../utils/direction";
import { Pair } from "../utils/types";
import { entityAt } from "./positioning";
import { assert } from "@thi.ng/api";

const group = createGroup(
  [components.transportLine, components.direction, components.position],
  `Transport line`
);

interface TransportLineSide {
  items: Array<{
    item: number;
    gap: number;
  }>;

  /**
   * The index of the first item that's not stuck
   */
  firstNonStuck: number;
}

interface TransportLine {
  sides: Pair<TransportLineSide>;

  start: Vec2Like;
  end: Vec2Like;

  steps: Direction[];
}

type TransportLineId = number;

const speed = 1;

export class TransportLineSystem {
  public transportLines: Record<TransportLineId, TransportLine> = {};

  private nextId = 0;

  public update() {
    for (const id in this.transportLines) {
      const line = this.transportLines[id];

      for (let sideIndex = 0; sideIndex < 2; sideIndex++) {
        const side = line.sides[sideIndex];
        const element = side.items[side.firstNonStuck];

        element.gap -= speed;

        if (element.gap < 0) {
          element.gap = 0;
          side.firstNonStuck++;
        }
      }
    }
  }

  public createLine(from: Vec2Like, to: Vec2Like, steps: Direction[]) {
    const id = this.nextId++;

    this.transportLines[id] = {
      start: from,
      end: to,
      steps,
      sides: [
        {
          items: [],
          firstNonStuck: -1,
        },
        {
          items: [],
          firstNonStuck: -1,
        },
      ],
    };

    return id;
  }

  public onEntityCreated(id: number) {
    const entity = group.getEntity(id);

    // Doens't have the necessary components
    if (entity === undefined) return;

    // Every direction we can move towards to try and find a belt pushing into the newly created one
    const possibleDirections = directions.filter(
      (d) => d !== entity.direction.direction
    );

    // Every position a belt pointing to the newly created one might be in
    const neighbours = possibleDirections
      .map((direction) => {
        const position = addDirection(entity.position, direction);
        const neighbourId = entityAt(position);

        // There's no entity there
        if (neighbourId === null) return null;

        const neighbour = group.getEntity(neighbourId);

        // The entity lacks one of the required components
        if (neighbour === undefined) return null;

        // The neighbour doesn't point towards us
        if (opposite(neighbour.direction.direction) !== direction) return null;

        assert(
          () => neighbour.transportLine.id !== null,
          `Belt somehow avoided getting a transport line added to it`
        );

        const line = this.transportLines[neighbour.transportLine.id!];

        assert(
          () => line !== undefined,
          `Invalid transport line id ${neighbour.transportLine.id}`
        );

        assert(
          () => equals2(line.end, neighbour.position),
          `Belt pointing towards previously empty position cannot be anywhere but at the end of it's transport line`
        );

        return neighbour;
      })
      .filter((e) => e !== null) as Array<typeof entity>;

    if (neighbours.length === 0) {
      const id = this.createLine(entity.position, entity.position, []);

      entity.transportLine.id = id;
    }

    if (neighbours.length === 1) {
      const neighbour = neighbours[0];
      const line = this.transportLines[neighbour.transportLine.id!];

      line.end = entity.position;
      line.steps.push(neighbour.direction.direction);

      entity.transportLine.id = neighbour.transportLine.id;
    }
  }
}
