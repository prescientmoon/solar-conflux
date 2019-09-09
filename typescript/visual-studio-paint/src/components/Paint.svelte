<script>
  import CanvasStack from "./CanvasStack"

  export let renderingContext$
  export let width
  export let height
</script>

<CanvasStack
  canvases={[{ id: 'a-layer', output$: renderingContext$ }]}
  {width}
  {height} />
