import { IVector4 } from "../types/IVector2"

export const vector4ToColor = (vector: IVector4) => `rgba(${vector.join(",")})`

export const vector4ToHex = (vector: IVector4) =>
  `#${vector
    .slice(0, 3)
    .map(Math.floor)
    .map(num => num.toString(16))
    .map(value => (value.length === 1 ? `0${value}` : value))
    .join("")}`
