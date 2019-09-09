<script>
  import { onMount } from "svelte"

  export let width
  export let height
  export let id
  export let output$

  let canvas

  onMount(() => {
    output$.next(canvas.getContext("2d"))
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

<canvas bind:this={canvas} {id} {width} {height} />
