import { IVector2, IVector4 } from "../types/IVector2"
import { brushOptionTypes } from "../constants/brushOptionTypes"
import { vector4ToColor } from "../helpers/vectorToColor"
import { PersistentSubject } from "rxjs-extra"
import { OptionBrush } from "./OptionBrush"
import { clearCanvas } from "../helpers/clearCanvas"
import { cursorOutOfBounds } from "../helpers/cursorOutOfBounds"
import { BrushMouseHandlerArguments, IBrush } from "../types/IBrush"

export class Brush extends OptionBrush implements IBrush {
  public icon = "brush"

  public constructor(name = "brush") {
    super(name)
  }

  public options = {
    color: {
      type: brushOptionTypes.color,
      value$: new PersistentSubject<IVector4>(this.getOptionKey("color"), [
        255,
        0,
        0,
        1
      ]),
      hidden: false
    },
    size: {
      type: brushOptionTypes.range,
      value$: new PersistentSubject(this.getOptionKey("size"), 3)
    },
    "cursor border": {
      type: brushOptionTypes.boolean,
      value$: new PersistentSubject(this.getOptionKey("cursor border"), true),
      hidden: false
    }
  }

  private lastPosition: IVector2 = [0, 0]

  public mouseDown(data: BrushMouseHandlerArguments) {
    if (data.state & 1) {
      this.lastPosition = data.position
    }

    this.draw(data)
  }

  private draw({
    contexts,
    position,
    state,
    painting
  }: BrushMouseHandlerArguments) {
    const [layer, cursor] = contexts

    const color = vector4ToColor(this.options.color.value$.value)
    const size = this.options.size.value$.value
    const cursorRadius = size / 2

    clearCanvas(cursor)

    if (
      this.options["cursor border"].value$.value &&
      (painting.settings.get("skip cursor collision checks").value ||
        !cursorOutOfBounds(cursor, position, cursorRadius))
    ) {
      cursor.strokeStyle = color
      cursor.beginPath()
      cursor.arc(position[0], position[1], cursorRadius, 0, 2 * Math.PI)

      cursor.stroke()
    }

    if (state & 1) {
      layer.beginPath()

      layer.lineCap = "round"
      layer.lineWidth = size
      layer.strokeStyle = color

      layer.moveTo(...this.lastPosition)
      layer.lineTo(...position)

      layer.stroke()
    }

    this.lastPosition = position
  }

  public mouseMove(args: BrushMouseHandlerArguments) {
    this.draw(args)
  }
}
