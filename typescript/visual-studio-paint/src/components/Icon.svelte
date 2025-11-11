<script>
  import { iconTypes } from "../constants/iconTypes"

  export let size = 27
  export let config = ""
</script>

<style>
  svg {
    position: relative;
    overflow: hidden;
    vertical-align: middle;
    -o-object-fit: contain;
    object-fit: contain;
    -webkit-transform-origin: center center;
    transform-origin: center center;
    stroke: currentColor;
    stroke-width: 2;
    stroke-linecap: round;
    stroke-linejoin: round;
    fill: none;
  }
</style>

{#if typeof config === 'string'}
  <div class="material-icons">{config}</div>
{:else if config.type === iconTypes.material}
  <div class="material-icons">{config.data}</div>
{:else if config.type === iconTypes.svg}
  <svg height={size} width={size}>
    <path d={config.data} />
  </svg>
{/if}
