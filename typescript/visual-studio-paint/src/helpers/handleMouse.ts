import { toLocalSpace } from "./toLocalSpace"
import { Painting } from "../classes/Painting"
import { take } from "rxjs/operators"
import { BehaviorSubject } from "rxjs"

export const createMouseHandler = (painting: Painting) => {
  let mouseState = new BehaviorSubject(0)

  return {
    down: async (e: MouseEvent) => {
      mouseState.next(mouseState.value | (1 << e.button))

      if (painting.currentBrush.mouseDown) {
        const contexts = await painting.contextsBehavior$
          .pipe(take(1))
          .toPromise()

        const position = toLocalSpace(contexts[0], [e.clientX, e.clientY])

        painting.currentBrush.mouseDown({
          contexts,
          state: mouseState.value,
          position,
          painting
        })
      }
    },
    move: async (e: MouseEvent) => {
      if (painting.currentBrush.mouseMove) {
        const contexts = await painting.contextsBehavior$
          .pipe(take(1))
          .toPromise()

        const position = toLocalSpace(contexts[0], [e.clientX, e.clientY])

        painting.currentBrush.mouseMove({
          contexts,
          state: mouseState.value,
          position,
          painting
        })
      }
    },
    up: async (e: MouseEvent) => {
      if (mouseState.value & (1 << e.button)) {
        mouseState.next(mouseState.value ^ (1 << e.button))
      }

      if (painting.currentBrush.mouseUp) {
        const contexts = await painting.contextsBehavior$
          .pipe(take(1))
          .toPromise()
        const position = toLocalSpace(contexts[0], [e.clientX, e.clientY])

        painting.currentBrush.mouseUp({
          contexts,
          state: mouseState.value,
          position,
          painting
        })
      }
    },
    state: () => mouseState
  }
}
