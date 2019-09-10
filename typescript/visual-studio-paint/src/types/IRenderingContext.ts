import { Subject } from "rxjs"

export interface IRenderingContext {
  id: string
  output$: Subject<CanvasRenderingContext2D>
}
