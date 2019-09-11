import { Painting } from "../classes/Painting"

export const clearCanvas = (
  context: CanvasRenderingContext2D,
  painting: Painting
) => {
  context.fillStyle = painting.settings.get("canvasBackground", "white").value
  context.fillRect(0, 0, context.canvas.width, context.canvas.height)
}
