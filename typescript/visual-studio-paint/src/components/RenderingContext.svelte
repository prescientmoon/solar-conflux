<script>
  import { onMount } from "svelte"

  export let width
  export let height
  export let id
  export let output$
  export let mouseHandler
  export let background = "white"

  let canvas

  onMount(() => {
    const context = canvas.getContext("2d")

    output$.next(context)

    // context.translate(100, 100)
    // context.scale(0.3, 0.3)
    // context.rotate(Math.PI / 4)
    // context.translate(400, -200)

    context.fillStyle = background
    context.fillRect(0, 0, width, height)
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
