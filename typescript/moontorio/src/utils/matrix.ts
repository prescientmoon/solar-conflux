import {
  decodeArray,
  decodeMatrix,
  Encoder,
  IFromJson,
  IToJson,
  Json,
} from "./json";
import { Fn, Vec2 } from "./types";

// ========= Isomorphisms
export interface Isomorphism<F, T> {
  do: Fn<F, T>;
  undo: Fn<T, F>;
}

export const isomorphismTransitivity = <A, B, C>(
  first: Isomorphism<A, B>,
  second: Isomorphism<B, C>
): Isomorphism<A, C> => ({
  do(input) {
    return second.do(first.do(input));
  },
  undo(output) {
    return first.undo(second.undo(output));
  },
});

type Integer = number;
type Natural = number;

export const intToNat: Isomorphism<Integer, Natural> = {
  do(integer) {
    return integer < 0 ? -2 * integer - 1 : 2 * integer;
  },
  undo(natural) {
    return natural % 2 ? (natural + 1) / -2 : natural / 2;
  },
};

export const vecToInt = (width: number): Isomorphism<Vec2, number> => ({
  do(vec2) {
    return vec2[0] + vec2[1] * width;
  },
  undo(num) {
    return [num % width, Math.floor(num / width)];
  },
});

// ========== Helpers
const iterableNegativeArray = function* <T>(arr: T[]): Generator<[T, number]> {
  const length = arr.length;

  const minPlacement = length % 2 ? length - 2 : length - 1;

  for (let x = minPlacement; x > 0; x -= 2) {
    yield [arr[x], x];
  }

  for (let x = 0; x < length; x += 2) {
    yield [arr[x], x];
  }
};

// ========== Matrices
export class FiniteMatrix<T> implements IToJson, IFromJson, Iterable<T> {
  public elements: T[] = [];
  public indices: Isomorphism<Vec2, number>;

  public constructor(
    public width: number,
    public heigth: number,
    public encodeElement: Encoder<T>,
    public decodeElement: (element: Json, position: Vec2) => T
  ) {
    this.indices = vecToInt(width);
    this.elements = Array(width * heigth).fill(null);
  }

  public get(index: Vec2) {
    return this.elements[this.indices.do(index)];
  }

  public set(index: Vec2, value: T) {
    this.elements[this.indices.do(index)] = value;
  }

  public encode() {
    return this.elements.map(this.encodeElement);
  }

  public decode(json: Json) {
    const arr = decodeArray((a) => a)(json);

    for (let index = 0; index < arr.length; index++) {
      const element = arr[index];

      this.elements[index] = this.decodeElement(
        element,
        this.indices.undo(index)
      );
    }
  }

  public *[Symbol.iterator]() {
    yield* this.elements;
  }
}

export class InfiniteMatrix<T extends IToJson & IFromJson>
  implements IToJson, IFromJson, Iterable<T> {
  public elements: T[][] = [];
  public indices: Isomorphism<Vec2, Vec2> = {
    do(input) {
      return [intToNat.do(input[0]), intToNat.do(input[1])];
    },
    undo(input) {
      return [intToNat.undo(input[0]), intToNat.undo(input[1])];
    },
  };

  public constructor(private createElement: (position: Vec2) => T) {}

  public get(index: Vec2): T | undefined {
    const nat = this.indices.do(index);

    return this.elements[nat[0]]?.[nat[1]];
  }

  public set(index: Vec2, value: T) {
    const nat = this.indices.do(index);

    let row = this.elements[nat[0]];

    if (row === undefined) {
      row = [];
      this.elements[nat[0]] = row;
    }

    row[nat[1]] = value;
  }

  public encode() {
    return this.elements.map((s) => s.map((e) => e.encode()));
  }

  public decode(json: Json) {
    const mat = decodeMatrix((a) => a)(json);

    for (const [row, x] of iterableNegativeArray(mat)) {
      if (!row) continue;

      this.elements[x] = [];

      for (const [element, y] of iterableNegativeArray(row)) {
        const inner = this.createElement(this.indices.undo([x, y]));

        this.elements[x][y] = inner;

        inner.decode(element);
      }
    }
  }

  public *[Symbol.iterator]() {
    for (const [row] of iterableNegativeArray(this.elements)) {
      if (row === undefined) continue;
      for (const [element] of iterableNegativeArray(row)) {
        if (element === undefined) continue;
        yield element;
      }
    }
  }
}
