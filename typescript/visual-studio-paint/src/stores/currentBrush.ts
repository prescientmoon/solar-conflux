import { BehaviorSubject } from "rxjs"
import { IBrush } from "../types/IBrush"

export const currentBrush$ = new BehaviorSubject<IBrush | null>(null)
