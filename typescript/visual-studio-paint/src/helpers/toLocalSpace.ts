import { IVector2 } from "../types/IVector2"
import { substract } from "./bascVector2"

export const toLocalSpace = (
  context: CanvasRenderingContext2D,
  position: IVector2
): IVector2 => {
  const bounds = context.canvas.getBoundingClientRect()
  const relativeToCanvasBounds = substract(position, [bounds.left, bounds.top])

  const matrix = context.getTransform()
  const imatrix = matrix.invertSelf()

  const x =
    relativeToCanvasBounds[0] * imatrix.a +
    relativeToCanvasBounds[1] * imatrix.c +
    imatrix.e
  const y =
    relativeToCanvasBounds[0] * imatrix.b +
    relativeToCanvasBounds[1] * imatrix.d +
    imatrix.f

  return [x, y]
}
