import { Painting } from "../classes/Painting"
import { resizeLayers } from "./resizeLayers"
import { take } from "rxjs/operators"

export const handleImageUpolad = (painting: Painting, file: File) => {
  const reader = new FileReader()

  reader.onload = (event: Event) => {
    if (!event) {
      throw new Error(`Reader onload event is undefined`)
    }

    const loadedImage = new Image()

    loadedImage.onload = async () => {
      const contexts = await painting.contextsBehavior$
        .pipe(take(1))
        .toPromise()

      resizeLayers(contexts, loadedImage.width, loadedImage.height)

      contexts[0].drawImage(loadedImage, 0, 0)
    }

    if (typeof reader.result === "string") {
      loadedImage.src = reader.result
    } else {
      throw new Error(`Image src is not a string`)
    }
  }

  reader.readAsDataURL(file)
}
