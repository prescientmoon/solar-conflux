import { equals2, stridedValues, Vec2Like } from "@thi.ng/vectors";
import { components, createGroup } from "../ecs";
import {
  addDirection,
  Direction,
  fromPositions,
  opposite,
  relativeTo,
} from "../utils/direction";
import { Pair, Side } from "../utils/types";
import { entityAt, neighbours } from "./positioning";
import { assert } from "@thi.ng/api";
import { getBeltLength } from "./beltCurving";

const group = createGroup(
  [components.transportLine, components.direction, components.position],
  `Transport line`
);

interface TransportLineSide {
  items: Array<{
    item: number;
    gap: number;
  }>;

  length: number;

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
  entities: number[];
}

type TransportLineId = number;

const speed = 1;
const spacePerItem = 2;

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

  public createLine(
    from: Vec2Like,
    to: Vec2Like,
    steps: Direction[],
    entities: number[],
    lengths: Pair<number> = [32, 32]
  ) {
    const id = this.nextId++;

    this.transportLines[id] = {
      start: from,
      end: to,
      steps,
      entities,
      sides: [
        {
          items: [],
          firstNonStuck: -1,
          length: lengths[0],
        },
        {
          items: [],
          firstNonStuck: -1,
          length: lengths[1],
        },
      ],
    };

    return id;
  }

  public getLine(id: TransportLineId): TransportLine {
    const line = this.transportLines[id];

    assert(() => line !== undefined, `Invalid transport line id ${id}`);

    return line;
  }

  public updateEntity(id: number) {
    const entity = group.getEntity(id);

    // Doens't have the necessary components
    if (entity === undefined) return;

    console.log(`Updating ${entity.position}`);

    const oldId = entity.transportLine.id;

    // Every position a belt pointing to the newly created one might be in
    const inputs = neighbours(id)
      .map(({ id, direction }) => {
        if (direction === entity.direction.direction) return null;

        const neighbour = group.getEntity(id);

        // The entity lacks one of the required components
        if (neighbour === undefined) return null;

        // The neighbour doesn't point towards us
        if (opposite(neighbour.direction.direction) !== direction) return null;

        assert(
          () => neighbour.transportLine.id !== null,
          `Belt somehow avoided getting a transport line added to it`
        );

        return neighbour;
      })
      .filter((e) => e !== null) as Array<typeof entity>;

    if (inputs.length === 0) {
      if (entity.transportLine.id === null) {
        const id = this.createLine(
          entity.position,
          entity.position,
          [],
          [entity.id]
        );

        entity.transportLine.id = id;
      } else {
        const line = this.getLine(entity.transportLine.id);

        assert(
          () => equals2(line.start, entity.position),
          `A conveyor belt with no inputs and an id attached to it cannot be anywhere but at the start of a transport line`
        );

        return;
      }
    }

    if (inputs.length === 1) {
      const neighbour = inputs[0];

      if (entity.transportLine.id === null) {
        const id = this.createLine(
          entity.position,
          entity.position,
          [],
          [entity.id]
        );

        entity.transportLine.id = id;
      }

      console.log({ entity, neighbour });

      this.merge(neighbour.transportLine.id!, entity.transportLine.id!);
    }

    if (inputs.length > 1) {
      if (entity.transportLine.id === null) {
        const id = this.createLine(
          entity.position,
          entity.position,
          [],
          [entity.id]
        );

        entity.transportLine.id = id;
      } else {
        const line = this.getLine(entity.transportLine.id);

        if (equals2(line.start, entity.position)) {
          return;
        }

        this.split(entity.transportLine.id, id, entity.position);
      }
    }

    if (oldId === entity.transportLine.id) return;

    const nextId = entityAt(
      addDirection(entity.position, entity.direction.direction)
    );

    if (nextId !== null) this.updateEntity(nextId);
  }

  private merge(idMerge: TransportLineId, idKeep: TransportLineId) {
    // Avoid circular chains
    if (idKeep === idMerge) return;

    const first = this.getLine(idMerge);
    const second = this.getLine(idKeep);
    const bridge = fromPositions(first.end, second.start);

    assert(
      () => bridge !== null,
      `Cannot merge 2 transport lines which are not next to eachother`
    );

    second.entities.unshift(...first.entities);
    second.steps.unshift(...first.steps, bridge!);
    second.start = first.start;

    for (let side = 0; side < 2; side++) {
      const sideFirst = first.sides[side];
      const sideSecond = second.sides[side];

      sideSecond.firstNonStuck =
        sideSecond.firstNonStuck >= sideSecond.items.length
          ? sideFirst.firstNonStuck
          : sideSecond.firstNonStuck;

      sideSecond.length += sideFirst.length;

      if (sideFirst.items.length) {
        console.log(`Gotta implement this thingy`);
      }

      sideSecond.items.push(...sideFirst.items);
    }

    // Cleanup for the old line
    for (const entity of first.entities) {
      components.transportLine.get(entity)!.id = idKeep;
    }

    Reflect.deleteProperty(this.transportLines, idMerge);
  }

  /**
   * Split a transport line in 2 smaller ones.
   *
   * @param id The id of the transport line to split.
   * @param at The position to do the split at. This entity will be the head of the second half after the split.
   */
  private split(id: TransportLineId, entityId: number, at: Vec2Like) {
    const line = this.getLine(id);

    if (equals2(line.start, at)) return;

    let index = null;

    {
      let position = line.start;

      for (let i = 0; i < line.steps.length; i++) {
        position = addDirection(position, line.steps[i]);

        if (equals2(at, position)) {
          index = i;
          break;
        }
      }
    }

    assert(index !== null);

    const firstHalfSteps = line.steps.splice(0, index! + 1);
    const bridge = firstHalfSteps.pop();

    assert(
      bridge !== undefined,
      `Somehow managed to get started on splitting a transport line in it's head?!?!?!?!?`
    );

    const splitEntityIndex = line.entities.indexOf(entityId);

    assert(
      splitEntityIndex !== -1,
      `Cannot find entity id ${entityId} in the entity array of the linetranpsort line being split`
    );

    const firstHalfEntities = line.entities.splice(0, splitEntityIndex);
    const lengths = [Side.Left, Side.Right].map((side) =>
      line.entities.reduce(
        (previous, current) => previous + getBeltLength(current, side),
        0
      )
    ) as Pair<number>;

    const newId = this.createLine(
      line.start,
      addDirection(at, opposite(bridge!)),
      firstHalfSteps,
      firstHalfEntities,
      lengths
    );

    const newLine = this.getLine(newId);

    for (const id of newLine.entities) {
      components.transportLine.get(id)!.id = newId;
    }

    line.start = at;

    for (let sideIndex: Side = 0; sideIndex < 2; sideIndex++) {
      const side = line.sides[sideIndex];
      side.length -= lengths[sideIndex];

      if (side.items.length === 0) continue;

      let itemSplitIndex = null;
      let gapDelta = null;

      {
        let distance = 0;

        for (let index = 0; index < side.items.length; index++) {
          if (distance + side.items[index].gap >= lengths[sideIndex]) {
            itemSplitIndex = index;
            gapDelta = side.items[index].gap + lengths[sideIndex] - sideIndex;
            break;
          }
          distance += side.items[index].gap + spacePerItem;
        }
      }

      assert(itemSplitIndex !== null);

      newLine.sides[sideIndex].items = side.items.splice(0, itemSplitIndex!);

      if (side.items.length) {
        assert(gapDelta !== null);
        assert(gapDelta! >= 0);

        side.items[0].gap = gapDelta!;
      }
    }

    this.fixExCycles(id, newId);
  }

  private fixExCycles(firstId: TransportLineId, secondId: TransportLineId) {
    const first = this.getLine(firstId);
    const second = this.getLine(secondId);
    const bridge = fromPositions(first.end, second.start);

    if (bridge !== null) this.merge(firstId, secondId);
  }
}
