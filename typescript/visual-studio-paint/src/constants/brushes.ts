import { IBrush } from "../types/IBrush"
import { Brush, Bucket } from "../classes/Brush"
import { currentBrush$ } from "../stores/currentBrush"

const brushes: IBrush[] = process.browser ? [new Brush(), new Bucket()] : []

currentBrush$.next(brushes[0])

export { brushes }
