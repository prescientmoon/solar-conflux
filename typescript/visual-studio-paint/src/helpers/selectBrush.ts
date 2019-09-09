import { BehaviorSubject } from "rxjs"
import { IBrush } from "../types/IBrush"

export const createBrushSelectionHandler = (
  currentBrush$: BehaviorSubject<IBrush>,
  brush: IBrush
) => () => {
  if (brush === currentBrush$.value) {
    return
  }

  currentBrush$.value.disable(0)
  brush.enable(0)

  currentBrush$.next(brush)
}
