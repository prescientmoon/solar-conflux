<script>
  import CanvasStack from "./CanvasStack"

  import {
    drawingRenderingContext$,
    cursorRenderingContext$
  } from "../stores/renderingContext"

  export let width
  export let height
</script>

<CanvasStack
  canvases={[{ id: 'first-layer', output$: drawingRenderingContext$ }, { id: 'cursor-layer', output$: cursorRenderingContext$ }]}
  {width}
  {height} />
