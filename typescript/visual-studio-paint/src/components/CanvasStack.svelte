<script>
  import RenderingContext from "./RenderingContext"
  import { createMouseHandler } from "../helpers/handleMouse"
  import { getContext } from "svelte"
  import { cursors } from "../constants/cursors"
  import { getCursor } from "../helpers/getCursor"

  export let width = 100
  export let height = 100
  export let canvases = []

  const painting = getContext("painting")
  const { currentBrush$ } = painting

  painting.contexts$.next(canvases)

  const mouseHandler = createMouseHandler(painting)
  const mouseState$ = mouseHandler.state()
</script>

<style>
  #canvas-container {
    position: relative;
    overflow: hidden;
    width: 100vw;
    height: 100vh;
  }
</style>

<div
  id="canvas-container"
  style={`cursor: url("${$currentBrush$ ? `cursors/${getCursor(cursors[$currentBrush$.name], $mouseState$, -1)}` : ''}"), auto`}
  class={`full`}>
  {#each canvases as canvas, index}
    <RenderingContext
      clear={index === 0}
      {mouseHandler}
      {width}
      {height}
      {...canvas} />
  {/each}
</div>
