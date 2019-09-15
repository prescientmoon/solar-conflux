import { IButtonConfig } from "../types/IButtonConfig"
import { take } from "rxjs/operators"
import { clearCanvas } from "../helpers/clearCanvas"
import "regenerator-runtime/runtime"

export const selectionPanelButtonConfigs: IButtonConfig[] = [
  {
    icon: "delete",
    text: "clear canvas",
    action: async painting => {
      const firstLayer = (await painting.contextsBehavior$
        .pipe(take(1))
        .toPromise())[0]

      clearCanvas(firstLayer, painting)
    }
  },
  {
    icon: "clear",
    text: "clear storage",
    action: () => {
      localStorage.clear()
    }
  }
]
