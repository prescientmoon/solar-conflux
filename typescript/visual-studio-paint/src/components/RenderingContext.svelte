<script>
  import { getContext, onMount } from "svelte"
  import { clearCanvas } from "../helpers/clearCanvas"

  export let width
  export let height
  export let id
  export let output$
  export let mouseHandler
  export let clear = true

  const painting = getContext("painting")

  let canvas

  onMount(() => {
    const context = canvas.getContext("2d")

    output$.next(context)

    if (clear) {
      clearCanvas(context, painting)
    }
  })
</script>

<style>
  canvas {
    position: absolute;
    display: block;

    top: 0;
    left: 0;
  }
</style>

<canvas
  on:mousedown={mouseHandler.down}
  on:mousemove={mouseHandler.move}
  on:mouseup={mouseHandler.up}
  bind:this={canvas}
  {id}
  {width}
  {height} />
