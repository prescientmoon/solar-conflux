import { IBrush } from "../types/IBrush"
import { PersistentSubject } from "rxjs-extra"
import { BehaviorSubject } from "rxjs"
import { filter } from "rxjs/operators"
import { brushes } from "../constants/brushes"

const currentBrush$ = new BehaviorSubject<IBrush | null>(null)

if (process.browser) {
  const persistent = new PersistentSubject<string | null>("current brush", null)

  if (persistent.value) {
    currentBrush$.next(
      brushes.find(brush => brush.name === persistent.value) || null
    )
  }

  currentBrush$.pipe(filter(brush => !!brush)).subscribe(brush => {
    persistent.next(brush!.name)
  })
}

if (currentBrush$.value === null) {
  currentBrush$.next(brushes[0])
}

export { currentBrush$ }
