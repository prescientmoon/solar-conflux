import { IBrush } from "../types/IBrush"
import { Brush } from "../classes/Brush"
import { Bucket } from "../classes/Bucket"
import { Eraser } from "../classes/Eraser"
import { Picker } from "../classes/Picker"

const brushes: IBrush[] = process.browser
  ? [new Brush(), new Bucket(), new Eraser(), new Picker()]
  : []

export { brushes }
