import * as sapper from "@sapper/app"
import { renderingContext$ } from "./stores/renderingContext"
import { interval, combineLatest } from "rxjs"
import "./constants.scss"

sapper.start({
  target: document.querySelector("#sapper")!
})

const loop = interval(100)

combineLatest(loop, renderingContext$).subscribe(([time, context]) => {
  context.fillStyle = "red"
  context.fillRect(300, 30, 100, 100)
})
