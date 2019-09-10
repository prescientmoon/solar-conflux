import { IVector2 } from "../types/IVector2"

export const add = (a: IVector2, b: IVector2) => {
  return a.map((value, index) => value + b[index]) as IVector2
}

export const substract = (a: IVector2, b: IVector2) => {
  return a.map((value, index) => value - b[index]) as IVector2
}
