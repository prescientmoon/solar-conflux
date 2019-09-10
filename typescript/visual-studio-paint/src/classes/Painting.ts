import { IRenderingContext } from "../types/IRenderingContext"
import { BehaviorSubject, combineLatest } from "rxjs"
import { IBrush } from "../types/IBrush"

export class Painting {
  public constructor(
    public currentBrush$: BehaviorSubject<IBrush>,
    public contexts: IRenderingContext[],
    public brushes: IBrush[]
  ) {}

  public rawContexts$() {
    const contexts$ = new BehaviorSubject<CanvasRenderingContext2D[] | null>(
      null
    )

    combineLatest<CanvasRenderingContext2D[]>(
      ...this.contexts.map(context => context.output$)
    ).subscribe(contexts$.next.bind(contexts$))

    return contexts$
  }

  public get currentBrush() {
    return this.currentBrush$.value
  }
}
