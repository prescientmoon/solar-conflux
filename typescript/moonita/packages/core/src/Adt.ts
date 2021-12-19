/** Js representation of purescript variants */
export type Adt<T extends object> = {
  [key in keyof T]: { type: key } & T[key];
}[keyof T];
