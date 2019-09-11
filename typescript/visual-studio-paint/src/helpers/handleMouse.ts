import { toLocalSpace } from "./toLocalSpace"
import { Painting } from "../classes/Painting"
import { take } from "rxjs/operators"

export const createMouseHandler = (painting: Painting) => {
  let mouseState = 0

  return {
    down: async (e: MouseEvent) => {
      mouseState |= 1 << e.button

      if (painting.currentBrush.mouseDown) {
        const contexts = await painting.contextsBehavior$
          .pipe(take(1))
          .toPromise()

        const position = toLocalSpace(contexts[0], [e.clientX, e.clientY])

        painting.currentBrush.mouseDown(contexts, mouseState, position)
      }
    },
    move: async (e: MouseEvent) => {
      if (painting.currentBrush.mouseMove) {
        const contexts = await painting.contextsBehavior$
          .pipe(take(1))
          .toPromise()

        const position = toLocalSpace(contexts[0], [e.clientX, e.clientY])

        painting.currentBrush.mouseMove(contexts, mouseState, position)
      }
    },
    up: async (e: MouseEvent) => {
      if (mouseState & (1 << e.button)) {
        mouseState ^= 1 << e.button
      }

      if (painting.currentBrush.mouseUp) {
        const contexts = await painting.contextsBehavior$
          .pipe(take(1))
          .toPromise()
        const position = toLocalSpace(contexts[0], [e.clientX, e.clientY])

        painting.currentBrush.mouseUp(contexts, mouseState, position)
      }
    }
  }
}
