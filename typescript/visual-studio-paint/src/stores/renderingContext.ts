import { BehaviorSubject } from "rxjs"

export const drawingRenderingContext$ = new BehaviorSubject<CanvasRenderingContext2D | null>(
  null
)

export const cursorRenderingContext$ = new BehaviorSubject<CanvasRenderingContext2D | null>(
  null
)
