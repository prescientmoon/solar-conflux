import { IButtonConfig } from "../types/IButtonConfig"
import { Painting } from "../classes/Painting"
import { saveAs } from "file-saver"

export const filePanelButtonConfigs: IButtonConfig[] = [
  {
    icon: "cloud_download",
    text: "download",
    action: (painting: Painting) => {
      const context = painting.contexts$.value[0].output$.value
      const { width, height } = context.canvas

      const downloadWidth = window.innerWidth
      const downloadHeight = window.innerHeight

      const data = context.getImageData(0, 0, downloadWidth, downloadHeight)

      context.canvas.width = downloadWidth
      context.canvas.height = downloadHeight

      context.putImageData(data, 0, 0)

      context.canvas.toBlob(blob => {
        if (blob) {
          saveAs(blob, "image.png")
        }

        context.canvas.width = width
        context.canvas.height = height

        context.putImageData(data, 0, 0)
      })
    }
  }
]
