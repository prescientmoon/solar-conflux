<script>
  import { firstCharUppercase } from "../helpers/firstCharUppercase"
  import { brushOptionTypes } from "../constants/brushOptionTypes"
  import { vector4ToColor, vector4ToHex } from "../helpers/vectorToColor"
  import { HsvPicker } from "svelte-color-picker"
  import { fly, slide } from "svelte/transition"
  import Switch from "@smui/switch"

  export let name = ""
  export let type = 0
  export let value$
  export let settings = {}

  let extended = false
  let optionValue = $value$

  $: value$.next(optionValue)
</script>

<style>
  #option {
    display: flex;
    width: 100%;
  }

  #option.color.extended {
    margin-bottom: var(--brush-option-spacing);
  }

  #option-name {
    flex-grow: 1;
  }

  #option-color-preview {
    width: 1.5em;
    height: 1.5em;
  }

  #picker {
    display: flex;
    justify-content: center;
    width: 100%;
  }
</style>

<div id="option" class:extended class:color={type === brushOptionTypes.color}>
  <div id="option-name">{firstCharUppercase(name)}:</div>

  {#if type === brushOptionTypes.color}
    <div
      id="option-color-preview"
      style={`background-color: ${vector4ToColor($value$)}`}
      on:click={() => {
        extended = !extended
      }} />
  {:else if type === brushOptionTypes.range}
    <input
      bind:value={optionValue}
      type="range"
      min={settings.min || 1}
      max={settings.max || 100}
      class="slider reset" />
  {:else if type === brushOptionTypes.boolean}
    <Switch bind:checked={optionValue} />
  {/if}

</div>

{#if type === brushOptionTypes.color && extended}
  <div id="picker" transition:slide>
    <HsvPicker
      on:colorChange={event => {
        const { r, g, b, a } = event.detail
        value$.next([r, g, b, a])
      }}
      startColor={vector4ToHex($value$)} />
  </div>
{/if}
