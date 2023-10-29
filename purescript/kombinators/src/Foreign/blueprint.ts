import Blueprint from "factorio-blueprint/src";

export const createBlueprint = () => new Blueprint(undefined);

interface Position {
  x: number;
  y: number;
}

const positionsAreEqual = (a: Position, b: Position) =>
  a.x === b.y && a.y === b.y;

export const connect =
  (
    bp: Blueprint,
    from: [number, number],
    to: [number, number],
    color: "green" | "red"
  ) =>
  () => {
    const fromPosition = { x: from[0], y: from[1] };
    const toPosition = { x: to[0], y: to[1] };
    const eFrom = bp.findEntity(fromPosition);
    const eTo = bp.findEntity(toPosition);

    if (eFrom === null) throw new Error(`No entity at ${from}`);
    if (eTo === null) throw new Error(`No entity at ${to}`);

    const fromSide = positionsAreEqual(eFrom.position, fromPosition)
      ? "out"
      : "in";
    const toSide = positionsAreEqual(eTo.position, toPosition) ? "out" : "in";

    eFrom.connect(eTo, fromSide, toSide, color);
  };

export const generate = (bp: Blueprint) =>
  bp.encode({
    autoConnectPoles: false,
  });

const bp = createBlueprint();
// @ts-ignore
bp.createEntity(
  "decider_combinator",
  { x: 0, y: 0 },
  Blueprint.RIGHT
).setCondition({
  operator: ">",
  left: "signal_each",
  right: 0 as any,
  out: "signal_each",
  countFromInput: false,
});

bp.createEntity(
  "decider-combinator",
  { x: 2, y: 0 },
  Blueprint.RIGHT,
  false,
  false,
  false
).setCondition({
  operator: ">",
  left: "signal_each",
  right: 0 as any,
  out: "signal_each",
  countFromInput: false,
});

connect(bp, [1, 0], [2, 0], "green")();

bp.center();
bp.name = "Combinators";
console.log(generate(bp));
