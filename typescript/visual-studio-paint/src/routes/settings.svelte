<script>
  import { getContext } from "svelte"
  import { settings } from "../constants/settings"
  import Option from "../components/Option"

  const painting = getContext("painting")
</script>

<style>
  .header {
    margin-bottom: var(--settings-spacing);
  }

  .settings {
    display: flex;
    flex-direction: column;
  }

  .setting {
    margin: var(--settings-spacing);
  }
</style>

<div class="header">Settings:</div>

<div class="settings">
  {#each Array.from(painting.settings.settingMap) as [name, value$]}
    <div class="setting">
      <Option {name} {...settings[name]} {value$} />
    </div>
  {/each}

</div>
