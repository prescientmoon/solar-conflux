import { BehaviorSubject } from "rxjs"

export const renderingContext$ = new BehaviorSubject<CanvasRenderingContext2D | null>(
  null
)
