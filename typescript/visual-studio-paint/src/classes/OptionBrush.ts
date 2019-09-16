import { IBrush, BrushIcon } from "../types/IBrush"
import { createOptionKeyGetter } from "../helpers/getOptionKey"

export class OptionBrush implements IBrush {
  public icon: BrushIcon = "settings"
  public name: string

  public constructor(name: string) {
    this.name = name

    this.getOptionKey = createOptionKeyGetter(this.name)
  }

  protected getOptionKey: ReturnType<typeof createOptionKeyGetter>
}
