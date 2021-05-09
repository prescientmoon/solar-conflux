import { Direction, Directional, Fn, Pair } from "./types";

export type Json = number | boolean | string | null | Json[] | JsonObject;

export interface JsonObject extends Record<string, Json> {}

export type Decoder<T> = Fn<Json, T>;
type Undecode<T> = T extends Decoder<infer R> ? R : never;

export interface IToJson {
  encode(): Json;
}

// Only use this internally in this module!!!
const typeofDecoder = <T>(type: string): Decoder<T> => (a) => {
  if (typeof a === type) return (a as any) as T;

  throw new Error(`${a} is not a ${type}`);
};

export const decodeString: Decoder<string> = typeofDecoder(`string`);
export const decodeNumber: Decoder<number> = typeofDecoder(`number`);
export const decodeBoolean: Decoder<boolean> = typeofDecoder(`boolean`);

export const decodeArray = <T>(inner: Decoder<T>): Decoder<T[]> => (a) => {
  if (Array.isArray(a)) return a.map(inner);

  throw new Error(`${inner} is not an array`);
};

export const decodeMatrix = <T>(inner: Decoder<T>): Decoder<T[][]> =>
  decodeArray(decodeArray(inner));

export const decodeFixedArray = <T>(
  length: number,
  inner: Decoder<T>
): Decoder<T[]> => (value) => {
  const arr = decodeArray(inner)(value);

  if (arr.length !== length) {
    throw new Error(`Expected ${value} to have a length of ${length}`);
  }

  return arr;
};

export const decodeFixedMatrix = <T>(
  length: number,
  inner: Decoder<T>
): Decoder<T[][]> => decodeFixedArray(length, decodeFixedArray(length, inner));

export const decodePair = <T>(inner: Decoder<T>) =>
  decodeFixedArray(2, inner) as Decoder<Pair<T>>;
export const decodeDirectional = <T>(inner: Decoder<T>) =>
  (decodeFixedArray(4, inner) as unknown) as Decoder<Directional<T>>;

export const decodeRecord = <T extends Record<string, Decoder<unknown>>>(
  decoders: T
): Decoder<{ [K in keyof T]: Undecode<T[K]> }> => (val) => {
  if (typeof val !== `object` || val === null || Array.isArray(val))
    throw new Error(`${val} is not an object`);

  const result: Record<string, any> = {};

  for (const key in decoders) {
    if (!(key in val))
      throw new Error(`${val} doesnt have required propriety ${key}`);

    const decoded = decoders[key](val[key] as Json);

    result[key] = decoded;
  }

  return result as any;
};

export const decodeDirection: Decoder<Direction> = (v) => {
  const n = decodeNumber(v);

  if (n < 0 || n > 4) throw new Error(`${v} is not a vaild direction`);

  return n;
};

export const decodeNullable = <T>(inner: Decoder<T>): Decoder<T | null> => (
  value
) => {
  if (value === null) return value;

  return inner(value);
};

export const decodeOptionalField = <T>(
  name: string,
  inner: Decoder<T>
): Decoder<T | null> => (value) => {
  const obj: JsonObject = decodeRecord({})(value);
  const prop = obj[name];

  if (prop === undefined) return null;

  return inner(prop);
};

export const oneOf = <T>(...decoders: Decoder<T>[]): Decoder<T> => (json) => {
  for (const decoder of decoders) {
    try {
      const decoded = decoder(json);

      return decoded;
    } catch {
      continue;
    }
  }

  throw new Error(`Un-decodeable json ${json}`);
};
