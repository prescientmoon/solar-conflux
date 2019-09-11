import { IRenderingContext } from "../types/IRenderingContext"
import { BehaviorSubject, Subscription, Observable } from "rxjs"
import { IBrush } from "../types/IBrush"
import { filter, map } from "rxjs/operators"
import { PersistentSubject } from "rxjs-extra"
import { Settings } from "./Settings"
import { IPaintingSettings } from "../types/IPaintingSettings"

export class Painting {
  public subscriptions: Subscription[] = []
  public contextsBehavior$: Observable<CanvasRenderingContext2D[]>
  public contexts$ = new BehaviorSubject<IRenderingContext[]>([])
  public settings = new Settings<IPaintingSettings>({
    canvasBackground: "white"
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
