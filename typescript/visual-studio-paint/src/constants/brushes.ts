import { IBrush } from "../types/IBrush"
import { Brush } from "../classes/Brush"
import { Bucket } from "../classes/Bucket"
import { Eraser } from "../classes/Eraser"

const brushes: IBrush[] = process.browser
  ? [new Brush(), new Bucket(), new Eraser()]
  : []

export { brushes }
