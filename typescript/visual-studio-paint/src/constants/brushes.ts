import { IBrush } from "../types/IBrush"
import { Brush, SomeOtherBrush } from "../classes/Brush"
import { currentBrush$ } from "../stores/currentBrush"

const brushes: IBrush[] = [new Brush(), new SomeOtherBrush()]

currentBrush$.next(brushes[0])

export { brushes }
