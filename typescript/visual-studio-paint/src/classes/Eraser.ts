import { Brush } from "./Brush"
import { IBrush, BrushMouseHandlerArguments } from "../types/IBrush"
import { IVector4 } from "../types/IVector2"
import { Painting } from "./Painting"
import { mdiEraserVariant } from "@mdi/js"
import { iconTypes } from "../constants/iconTypes"

export class Eraser extends Brush implements IBrush {
  public icon = {
    type: iconTypes.svg,
    data: mdiEraserVariant
  }

  public constructor() {
    super("eraser")

    this.options.color.hidden = true

    this.reset()
  }

  private reset(color: IVector4 = [255, 255, 255, 1]) {
    this.options["color"].value$.next(color)
  }

  private resetWithPainting(painting: Painting) {
    this.reset(painting.settings.get("canvas background").value)
  }

  public mouseDown(args: BrushMouseHandlerArguments) {
    this.resetWithPainting(args.painting)

    super.mouseDown(args)
  }

  public mouseMove(args: BrushMouseHandlerArguments) {
    this.resetWithPainting(args.painting)

    super.mouseMove(args)
  }
}
