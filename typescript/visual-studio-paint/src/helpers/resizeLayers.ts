export const resizeLayers = (
  contexts: CanvasRenderingContext2D[],
  width: number,
  height: number
) => {
  for (const context of contexts) {
    context.canvas.width = width
    context.canvas.height = height
  }
}
