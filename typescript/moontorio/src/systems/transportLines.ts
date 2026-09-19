import { equals2, Vec2, Vec2Like } from "@thi.ng/vectors";
import { BeltCurve, components, createGroup, Env } from "../ecs";
import {
  addDirection,
  Direction,
  directionToVector,
  fromPositions,
  next,
  onXAxis,
  opposite,
  prev,
  relativeTo,
} from "../utils/direction";
import { Pair, Side } from "../utils/types";
import { entityAt, neighbours } from "./positioning";
import { assert } from "@thi.ng/api";
import {
  entityInputDirection,
  getBeltCurve,
  getBeltLength,
  inputDirection,
} from "./beltCurving";
import {
  beltSpacePerItem,
  halfTile,
  sideFromMiddle,
  transportLineSizes,
} from "../settings";
import { last } from "../utils/array";

const group = createGroup(
  [components.transportLine, components.direction, components.position],
  `Transport line`
);

export type TransportLinePathSegment = {
  amount: number;
  direction: Direction;
};

interface TransportLineSide {
  items: Array<{
    item: number;
    gap: number;
  }>;

  /**
   * The length of the transport path.
   */
  length: number;

  /**
   * The index of the first item that's not stuck
   */
  firstNotStuck: number;

  path: TransportLinePathSegment[];
}

interface TransportLine {
  sides: Pair<TransportLineSide>;

  start: Vec2Like;
  end: Vec2Like;

  entities: number[];

  entry: Direction;
}

type TransportLineId = number;

const speed = 1;

export class TransportLineSystem implements Iterable<TransportLine> {
  public transportLines: Record<TransportLineId, TransportLine> = {};

  private nextId = 0;

  *[Symbol.iterator]() {
    yield* Object.values(this.transportLines);
  }

  public update(env: Env) {
    if (env.tick % 7) return;

    for (const id in this.transportLines) {
      const line = this.transportLines[id];

      for (let sideIndex = 0; sideIndex < 2; sideIndex++) {
        const side = line.sides[sideIndex];

        if (
          side.firstNotStuck === -1 ||
          side.firstNotStuck >= side.items.length
        )
          continue;

        const element = side.items[side.firstNotStuck];

        element.gap -= speed;

        if (element.gap < 0) {
          element.gap = 0;
          side.firstNotStuck++;
        }
      }
    }
  }

  public createLine(
    from: Vec2Like,
    to: Vec2Like,
    entities: number[],
    curves: BeltCurve[]
  ) {
    console.log(`Creating line`);
    console.log({ from, to, entities, curves });

    const id = this.nextId++;

    this.transportLines[id] = {
      start: from,
      end: to,
      entities,
      entry: 0,
      sides: [
        {
          items: [{ gap: 10, item: 1 }],
          firstNotStuck: 0,
          length: 0,
          path: [],
        },
        {
          items: [],
          firstNotStuck: -1,
          length: 0,
          path: [],
        },
      ],
    };

    this.rebuildPaths(id);

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
          [entity.id],
          [getBeltCurve(entity.id) ?? BeltCurve.NoCurve]
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
          [entity.id],
          [getBeltCurve(entity.id) ?? BeltCurve.NoCurve]
        );

        entity.transportLine.id = id;
      }

      this.merge(neighbour.transportLine.id!, entity.transportLine.id!);
    }

    if (inputs.length > 1) {
      if (entity.transportLine.id === null) {
        const id = this.createLine(
          entity.position,
          entity.position,
          [entity.id],
          [getBeltCurve(entity.id) ?? BeltCurve.NoCurve]
        );

        entity.transportLine.id = id;
      } else {
        const line = this.getLine(entity.transportLine.id);

        if (equals2(line.start, entity.position)) {
          return;
        }

        this.split(entity.transportLine.id, id);
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

    {
      const bridge = fromPositions(first.end, second.start);

      assert(
        () => bridge !== null,
        `Cannot merge 2 transport lines which are not next to eachother`
      );
    }

    this.rebuildPaths(idKeep);

    for (let side = 0; side < 2; side++) {
      const sideFirst = first.sides[side];
      const sideSecond = second.sides[side];

      sideSecond.firstNotStuck =
        sideSecond.firstNotStuck >= sideSecond.items.length
          ? sideFirst.firstNotStuck
          : sideSecond.firstNotStuck;

      if (sideFirst.items.length) {
        sideFirst.items[0].gap +=
          sideSecond.length -
          sideSecond.items.reduce(
            (acc, curr) => acc + beltSpacePerItem + curr.gap,
            0
          );

        console.log(
          sideSecond.length -
            sideSecond.items.reduce(
              (acc, curr) => acc + beltSpacePerItem + curr.gap,
              0
            )
        );
      }

      sideSecond.items.push(...sideFirst.items);
    }

    second.entities.unshift(...first.entities);
    second.start = first.start;

    this.rebuildPaths(idKeep);

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
  private split(id: TransportLineId, entityId: number) {
    const line = this.getLine(id);
    const at = components.position.get(entityId)!;

    assert(
      at !== undefined,
      `Cannot find entity ${entityId} while splitting transport line`
    );

    if (equals2(line.start, at)) return;

    const splitEntityIndex = line.entities.indexOf(entityId);

    assert(
      splitEntityIndex !== -1,
      `Cannot find entity id ${entityId} in the entity array of the linetranpsort line being split`
    );

    const firstHalfEntities = line.entities.splice(0, splitEntityIndex);

    const newId = this.createLine(
      line.start,
      components.position.get(last(firstHalfEntities))!,
      firstHalfEntities,
      firstHalfEntities.map((e) => getBeltCurve(e) ?? BeltCurve.NoCurve)
    );

    const newLine = this.getLine(newId);

    for (const id of newLine.entities) {
      components.transportLine.get(id)!.id = newId;
    }

    line.start = at;

    this.rebuildPaths(id);

    for (let sideIndex: Side = 0; sideIndex < 2; sideIndex++) {
      const side = line.sides[sideIndex];
      const newSide = newLine.sides[sideIndex];

      if (side.items.length === 0) continue;

      let itemSplitIndex = null;
      let gapDelta = null;

      {
        let distance = 0;

        for (let itemIndex = 0; itemIndex < side.items.length; itemIndex++) {
          if (distance + side.items[itemIndex].gap >= side.length) {
            itemSplitIndex = itemIndex;
            gapDelta = side.items[itemIndex].gap + distance - side.length;
            break;
          }

          distance += side.items[itemIndex].gap + beltSpacePerItem;
        }
      }

      assert(itemSplitIndex !== null);

      newLine.sides[sideIndex].items = side.items.splice(
        itemSplitIndex!,
        side.items.length - itemSplitIndex!
      );

      if (newSide.items.length) {
        assert(gapDelta !== null);
        assert(gapDelta! >= 0);

        newSide.items[0].gap = gapDelta!;
      }
    }

    this.fixExCycles(id, newId);
  }

  private refreshLengths(id: TransportLineId) {
    const line = this.transportLines[id];

    for (let side: Side = 0; side < 2; side++) {
      line.sides[side].length = pathLength(line.sides[side].path);
    }
  }

  private rebuildPaths(id: TransportLineId) {
    const line = this.transportLines[id];

    const curves = line.entities.map(
      (e) => components.beltCurve.get(e)?.curve ?? BeltCurve.NoCurve
    );

    const entry = opposite(entityInputDirection(line.entities[0]));
    const paths = fromCurves(entry, curves);

    line.entry = entry;

    for (let side: Side = 0; side < 2; side++) {
      line.sides[side].path = paths[side];
    }

    this.refreshLengths(id);
  }

  private fixExCycles(firstId: TransportLineId, secondId: TransportLineId) {
    const first = this.getLine(firstId);
    const second = this.getLine(secondId);
    const bridge = fromPositions(first.end, second.start);

    if (bridge) this.updateEntity(second.entities[0]);
  }
}

// ========= Transport line path helpers
const flipPath = (line: TransportLinePathSegment[]) =>
  line.map((step) => ({
    amount: step.amount,
    direction: onXAxis(step.direction)
      ? step.direction
      : opposite(step.direction),
  }));

const rotateTransportLinePath = (
  direction: Direction,
  line: TransportLinePathSegment[]
): TransportLinePathSegment[] =>
  line.map((step) => ({
    amount: step.amount,
    direction: relativeTo(step.direction, direction),
  }));

const fromCurves = (
  startingDirection: Direction,
  curves: BeltCurve[]
): Pair<TransportLinePathSegment[]> => {
  const result: Pair<TransportLinePathSegment[]> = [[], []];

  let currentDirection = startingDirection;

  // NOTE: rn this assumes starting w a straight line
  for (let index = 0; index < curves.length; index++) {
    const current = curves[index];

    if (current === BeltCurve.Right) currentDirection = next(currentDirection);
    if (current === BeltCurve.Left) currentDirection = prev(currentDirection);

    const piece = paths[current].map((path) =>
      rotateTransportLinePath(currentDirection, path)
    );

    result[0].push(...piece[0]);
    result[1].push(...piece[1]);
  }

  return result;
};

// ========= Transport line paths
const straightBeltPath: TransportLinePathSegment[] = [
  { amount: transportLineSizes.straight, direction: Direction.Right },
];

const bentRightPath = (fromMiddle: number): TransportLinePathSegment[] => [
  { amount: halfTile - fromMiddle, direction: Direction.Up },
  {
    amount: halfTile - fromMiddle,
    direction: Direction.Right,
  },
];

const paths: Record<BeltCurve, Pair<TransportLinePathSegment[]>> = {
  [BeltCurve.NoCurve]: [straightBeltPath, straightBeltPath],
  [BeltCurve.Right]: [
    bentRightPath(sideFromMiddle),
    bentRightPath(-sideFromMiddle),
  ],
  [BeltCurve.Left]: [
    flipPath(bentRightPath(-sideFromMiddle)),
    flipPath(bentRightPath(sideFromMiddle)),
  ],
};

const pathLength = (path: TransportLinePathSegment[]) => {
  let result = 0;

  for (const segment of path) {
    result += segment.amount;
  }

  return result;
};
