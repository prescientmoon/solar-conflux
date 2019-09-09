<script>
  import RenderingContext from "./RenderingContext"

  export let width = 100
  export let height = 100
  export let canvases = []
</script>

<style>
  #canvas-container {
    position: relative;
    overflow: hidden;
  }
</style>

<div id="canvas-container" class="full">
  {#each canvases as canvas, index}
    <RenderingContext
      {width}
      {height}
      id={canvas.id}
      output$={canvas.output$} />
  {/each}
</div>
