import { IBrush } from "../types/IBrush"
import { IVector2, IVector4 } from "../types/IVector2"
import { brushOptionTypes } from "../constants/brushOptionTypes"
import { BehaviorSubject } from "rxjs"
import { vector4ToColor } from "../helpers/vectorToColor"

export class Brush implements IBrush {
  public name = "brush"
  public icon = "brush"

  public options = {
    color: {
      type: brushOptionTypes.color,
      value: new BehaviorSubject<IVector4>([255, 0, 0, 1])
    },
    size: {
      type: brushOptionTypes.number,
      value: new BehaviorSubject(3)
    }
  }

  private lastPosition: IVector2 = [0, 0]

  public mouseDown(
    contexts: CanvasRenderingContext2D[],
    state: number,
    position: IVector2
  ) {
    if (state & 1) {
      this.lastPosition = position
    }
  }

  public mouseMove(
    contexts: CanvasRenderingContext2D[],
    state: number,
    position: IVector2
  ) {
    if (state & 1) {
      const firstLayer = contexts[0]

      firstLayer.beginPath()

      firstLayer.lineWidth = this.options.size.value.value
      firstLayer.strokeStyle = vector4ToColor(this.options.color.value.value)

      firstLayer.moveTo(...this.lastPosition)
      firstLayer.lineTo(...position)

      firstLayer.stroke()
    }

    this.lastPosition = position
  }
}

export class SomeOtherBrush implements IBrush {
  public name = "other"
  public icon = "edit"
}

export class Bucket implements IBrush {
  public name = "bucket"
  public icon = "perm_media"

  public filling = false

  public options = {
    color: {
      type: brushOptionTypes.color,
      value: new BehaviorSubject<IVector4>([255, 0, 0, 1])
    }
  }

  public mouseDown(
    contexts: CanvasRenderingContext2D[],
    state: number,
    originalPosition: IVector2
  ) {
    if (this.filling) {
      return
    } else if (state & 1) {
      this.filling = true
      const position = originalPosition.map(Math.ceil) as IVector2

      const pixelStack = [position]
      const { width: rawWidth, height: rawHeight } = contexts[0].canvas

      const width = Math.ceil(rawWidth)
      const height = Math.ceil(rawHeight)

      const colorLayer = contexts[0].getImageData(0, 0, rawWidth, rawHeight)

      const targetColor = this.options.color.value.value
      const startIndex = (position[0] + Math.ceil(width) * position[1]) * 4
      const startColor: IVector4 = [
        colorLayer.data[startIndex],
        colorLayer.data[startIndex + 1],
        colorLayer.data[startIndex + 2],
        colorLayer.data[startIndex + 3]
      ]

      if (
        targetColor[0] === startColor[0] &&
        targetColor[1] === startColor[1] &&
        targetColor[2] === startColor[2] &&
        targetColor[3] * 255 === startColor[3]
      ) {
        this.filling = false
        return
      }

      const colorPixel = (
        pixelPosition: number,
        fill: IVector4 = targetColor
      ) => {
        colorLayer.data[pixelPosition] = fill[0]
        colorLayer.data[pixelPosition + 1] = fill[1]
        colorLayer.data[pixelPosition + 2] = fill[2]
        colorLayer.data[pixelPosition + 3] = fill[3] * 255
      }

      const matchStartColor = (
        pixelPosition: number,
        color: IVector4 = startColor
      ) => {
        const r = colorLayer.data[pixelPosition]
        const g = colorLayer.data[pixelPosition + 1]
        const b = colorLayer.data[pixelPosition + 2]
        const a = colorLayer.data[pixelPosition + 3]

        return (
          r === color[0] && g === color[1] && b === color[2] && a === color[3]
        )
      }

      while (pixelStack.length) {
        let newPosition = pixelStack.pop()!,
          x = newPosition[0],
          y = newPosition[1],
          pixelPosition = (y * width + x) * 4,
          reachLeft = 0,
          reachRight = 0

        while (y-- >= 0 && matchStartColor(pixelPosition)) {
          pixelPosition -= width * 4
        }

        pixelPosition += width * 4

        ++y

        while (y++ < height - 1 && matchStartColor(pixelPosition)) {
          colorPixel(pixelPosition)

          if (x > 0) {
            if (matchStartColor(pixelPosition - 4)) {
              if (!reachLeft) {
                pixelStack.push([x - 1, y])
                reachLeft = 1
              }
            } else if (reachLeft) {
              reachLeft = 0
            }
          }

          if (x < width - 1) {
            if (matchStartColor(pixelPosition + 4)) {
              if (!reachRight) {
                pixelStack.push([x + 1, y])
                reachRight = 1
              }
            } else if (reachRight) {
              reachRight = 0
            }
          }

          pixelPosition += width * 4
        }
      }

      contexts[0].putImageData(colorLayer, 0, 0)
      this.filling = false
    }
  }
}
