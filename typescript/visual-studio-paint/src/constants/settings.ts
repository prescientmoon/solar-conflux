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
  },
  "skip cursor collision checks": {
    type: brushOptionTypes.boolean
  }
}
