import { Subject } from "rxjs"

export const renderingContext$ = new Subject<CanvasRenderingContext2D>()
