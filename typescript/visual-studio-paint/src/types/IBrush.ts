import { IVector2, IVector4 } from "./IVector2"
import { brushOptionTypes } from "../constants/brushOptionTypes"
import { BehaviorSubject } from "rxjs"

export type mouseState = number

export type mouseHandler = (
  contexts: CanvasRenderingContext2D[],
  state: mouseState,
  position: IVector2
) => void

export type BrushOption = {
  type: brushOptionTypes
  value:
    | BehaviorSubject<IVector4>
    | BehaviorSubject<string>
    | BehaviorSubject<number>
  settings?: {
    min?: number
    max?: number
    preview?: number
  }
}

export interface IBrush {
  name: string
  icon: string

  options?: Record<string, BrushOption>

  enable?(state: mouseState): mouseState | void
  disable?(state: mouseState): mouseState | void

  mouseDown?: mouseHandler
  mouseUp?: mouseHandler
  mouseMove?: mouseHandler
}
