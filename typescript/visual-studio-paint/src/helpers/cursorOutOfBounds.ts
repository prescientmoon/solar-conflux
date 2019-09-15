import { IVector2 } from "../types/IVector2"

export const cursorOutOfBounds = (
  context: CanvasRenderingContext2D,
  position: IVector2,
  radius: number
) => {
  const { width } = context.canvas

  return (
    position[0] <= radius ||
    position[1] <= radius ||
    position[0] >= width - radius * 2 ||
    position[1] >= window.innerHeight - radius
  )
}
