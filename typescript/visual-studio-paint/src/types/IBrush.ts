import { IVector2 } from "./IVector2"

export type mouseState = number

export type mouseHandler = (
  contexts: CanvasRenderingContext2D[],
  state: mouseState,
  position: IVector2
) => void

export interface IBrush {
  name: string
  icon: string

  enable(state: mouseState): mouseState | void
  disable(state: mouseState): mouseState | void

  mouseDown?: mouseHandler
  mouseUp?: mouseHandler
  mouseMove?: mouseHandler
}
