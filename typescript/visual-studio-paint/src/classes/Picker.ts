import { IBrush, BrushMouseHandlerArguments } from "../types/IBrush"
import { mdiEyedropperVariant } from "@mdi/js"
import { iconTypes } from "../constants/iconTypes"
import { brushes } from "../constants/brushes"
import { brushOptionTypes } from "../constants/brushOptionTypes"
import { IVector4 } from "../types/IVector2"
import { BehaviorSubject } from "rxjs"

export class Picker implements IBrush {
  public icon = {
    type: iconTypes.svg,
    data: mdiEyedropperVariant
  }

  public name = "color picker"

  public mouseDown({ contexts, position }: BrushMouseHandlerArguments) {
    const layer = contexts[0]

    const pixel = layer.getImageData(position[0], position[1], 1, 1).data

    for (const brush of brushes) {
      if (
        brush.options &&
        brush.options.color &&
        brush.options.color.type === brushOptionTypes.color
      ) {
        ;(brush.options.color.value$ as BehaviorSubject<IVector4>).next([
          pixel[0],
          pixel[1],
          pixel[2],
          pixel[3] / 255
        ])
      }
    }
  }
}
