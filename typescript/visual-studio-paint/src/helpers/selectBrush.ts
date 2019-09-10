import { BehaviorSubject } from "rxjs"
import { IBrush } from "../types/IBrush"

export const createBrushSelectionHandler = (
  currentBrush$: BehaviorSubject<IBrush>,
  brush: IBrush
) => () => {
  if (brush === currentBrush$.value) {
    return
  }

  if (currentBrush$.value.disable) {
    currentBrush$.value.disable(0)
  }

  if (brush.enable) {
    brush.enable(0)
  }

  for (const option in currentBrush$.value.options || {}) {
    if (
      brush.options &&
      brush.options[option] &&
      brush.options[option].type === currentBrush$.value.options![option]!.type
    ) {
      brush.options[option].value = currentBrush$.value.options![option]!.value
    }
  }

  currentBrush$.next(brush)
}
