import { IRenderingContext } from "../types/IRenderingContext"
import { BehaviorSubject, Subscription, Observable, of } from "rxjs"
import { IBrush } from "../types/IBrush"
import { filter, map } from "rxjs/operators"
import { Settings } from "./Settings"
import { IPaintingSettings } from "../types/IPaintingSettings"
import { IVector4 } from "../types/IVector2"

export class Painting {
  public subscriptions: Subscription[] = []
  public contextsBehavior$: Observable<CanvasRenderingContext2D[]> = of([])
  public contexts$ = new BehaviorSubject<IRenderingContext[]>([])

  public settings = new Settings<IPaintingSettings>({
    "canvas background": [255, 255, 255, 1] as IVector4,
    "skip cursor collision checks": false
  })

  public constructor(
    public currentBrush$: BehaviorSubject<IBrush>,
    public brushes: IBrush[]
  ) {
    this.contextsBehavior$ = this.contexts$.pipe(
      map(contexts => {
        return contexts.map(context => context.output$.value)
      }),
      filter(contexts => contexts.length === contexts.filter(Boolean).length)
    )
  }

  public get currentBrush() {
    return this.currentBrush$.value
  }
}
