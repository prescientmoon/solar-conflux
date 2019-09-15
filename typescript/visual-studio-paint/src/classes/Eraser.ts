import { Brush } from "./Brush"
import { IBrush, BrushMouseHandlerArguments } from "../types/IBrush"

export class Eraser extends Brush implements IBrush {
  public icon = "eraser"

  public constructor() {
    super("eraser")

    this.options.color.hidden = true

    this.reset()
  }

  private reset() {
    this.options["color"].value$.next([255, 255, 255, 1])
  }

  public mouseDown(args: BrushMouseHandlerArguments) {
    this.reset()

    super.mouseDown(args)
  }

  public mouseMove(args: BrushMouseHandlerArguments) {
    this.reset()

    super.mouseMove(args)
  }
}
