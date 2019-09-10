import { toLocalSpace } from "./toLocalSpace"
import { Painting } from "../classes/Painting"

export const createMouseHandler = (painting: Painting) => {
  let mouseState = 0

  const rawContexts$ = painting.rawContexts$()

  return {
    down: (e: MouseEvent) => {
      mouseState |= 1 << e.button

      if (painting.currentBrush.mouseDown) {
        const contexts = rawContexts$.value!
        const position = toLocalSpace(contexts[0], [e.clientX, e.clientY])

        painting.currentBrush.mouseDown(contexts, mouseState, position)
      }
    },
    move: (e: MouseEvent) => {
      if (painting.currentBrush.mouseMove) {
        const contexts = rawContexts$.value!
        const position = toLocalSpace(contexts[0], [e.clientX, e.clientY])

        painting.currentBrush.mouseMove(contexts, mouseState, position)
      }
    },
    up: (e: MouseEvent) => {
      if (mouseState & (1 << e.button)) {
        mouseState ^= 1 << e.button
      }

      if (painting.currentBrush.mouseUp) {
        const contexts = rawContexts$.value!
        const position = toLocalSpace(contexts[0], [e.clientX, e.clientY])

        painting.currentBrush.mouseUp(contexts, mouseState, position)
      }
    }
  }
}
