import { BehaviorSubject } from "rxjs"
import { IBrush } from "../types/IBrush"

export const createBrushSelectionHandler = (
  currentBrush$: BehaviorSubject<IBrush>,
  brush: IBrush
) => () => {
  const other = currentBrush$.value

  if (brush === other) {
    return
  }

  if (other.disable) {
    other.disable(0)
  }

  if (brush.enable) {
    brush.enable(0)
  }

  for (const option in currentBrush$.value.options || {}) {
    if (
      brush.options &&
      other.options &&
      brush.options[option] &&
      other.options[option] &&
      brush.options[option].hidden !== true &&
      other.options[option].hidden !== true &&
      brush.options[option].type === other.options[option].type
    ) {
      // this is so hacky...
      brush.options[option].value$.next(other.options[option].value$
        .value as never)
    }
  }

  currentBrush$.next(brush)
}
