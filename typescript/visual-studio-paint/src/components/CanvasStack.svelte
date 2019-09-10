<script>
  import RenderingContext from "./RenderingContext"
  import { createMouseHandler } from "../helpers/handleMouse"
  import { getContext } from "svelte"

  export let width = 100
  export let height = 100
  export let canvases = []

  const painting = getContext("painting")
  const { currentBrush$ } = painting

  painting.contexts = canvases

  const mouseHandler = createMouseHandler(painting)
</script>

<style>
  #canvas-container {
    position: relative;
    overflow: hidden;
  }
</style>

<div id="canvas-container" class="full">
  {#each canvases as canvas, index}
    <RenderingContext {mouseHandler} {width} {height} {...canvas} />
  {/each}
</div>
