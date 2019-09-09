<script>
  import BrushPanel from "../components/BrushPanel"
  import { currentBrush$ } from "../stores/currentBrush"
  import { brushes } from '../constants/brushes'
</script>

<BrushPanel {currentBrush$} {brushes}/>
