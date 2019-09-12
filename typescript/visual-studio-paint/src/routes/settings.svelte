<script>
  import { getContext } from "svelte"
  import { settings } from "../constants/settings"
  import Option from "../components/Option"

  const painting = getContext("painting")
</script>

<style>
    .header {
        margin-bottom: 1rem;
    }
</style>

<div class="header">Settings:</div>

{#each Array.from(painting.settings.settingMap) as [name, value]}
  <Option {name} {...settings[name]} {value} />
{/each}
