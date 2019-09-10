import { IBrush } from "../types/IBrush"
import { Brush, SomeOtherBrush, Bucket } from "../classes/Brush"
import { currentBrush$ } from "../stores/currentBrush"

const brushes: IBrush[] = [new Brush(), new SomeOtherBrush(), new Bucket()]

currentBrush$.next(brushes[0])

export { brushes }
