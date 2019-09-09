import { IBrush } from "../types/IBrush"

export class Brush implements IBrush {
  public constructor(public name = "brush", public icon = "brush") {}

  public enable() {
    console.log(`Enabled: ${this.name}`)
  }

  public disable() {
    console.log(`Disable: ${this.name}`)
  }
}

export class SomeOtherBrush implements IBrush {
  public constructor(public name = "other", public icon = "edit") {}

  public enable() {
    console.log(`Enabled: ${this.name}`)
  }

  public disable() {
    console.log(`Disable: ${this.name}`)
  }
}
