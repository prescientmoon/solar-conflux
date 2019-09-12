import { IBrush } from "../types/IBrush"
import { IPaintingSettings } from "../types/IPaintingSettings"
import { brushOptionTypes } from "./brushOptionTypes"

export const settings: Record<
  keyof IPaintingSettings,
  {
    type: brushOptionTypes
  }
> = {
  "canvas background": {
    type: brushOptionTypes.color
  }
}
