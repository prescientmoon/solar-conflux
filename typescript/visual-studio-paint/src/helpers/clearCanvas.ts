import { Painting } from "../classes/Painting"
import { vector4ToColor } from "./vectorToColor"

export const clearCanvas = (
  context: CanvasRenderingContext2D,
  painting?: Painting
) => {
  if (painting) {
    context.fillStyle = vector4ToColor(
      painting.settings.get("canvas background").value
    )

    context.fillRect(0, 0, context.canvas.width, context.canvas.height)
  } else {
    context.clearRect(0, 0, context.canvas.width, context.canvas.height)
  }
}
