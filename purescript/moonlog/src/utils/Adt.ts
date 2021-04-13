/** Js representation for asts */
export type Adt<T extends object> = {
  [key in keyof T]: { type: key; value: T[key] };
}[keyof T];
