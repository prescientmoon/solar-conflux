import { PersistentSubject } from "rxjs-extra"
import { BehaviorSubject } from "rxjs"

export interface IRenderingContext {
  id: string
  output$: BehaviorSubject<CanvasRenderingContext2D>
}
