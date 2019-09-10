<script>
  import List, { Item, Graphic, Text } from "@smui/list"

  export let buttons = []
</script>

<style>
  #buttons {
    --mdc-theme-text-primary-on-background: var(--panel-text-color);
    --mdc-theme-text-icon-on-background: var(--panel-icon-color);
  }
</style>

<div id="buttons">
  {#if process.browser}
    <List>
      {#each buttons as button, index}
        <Item on:SMUI:action={button.action || (() => {})}>
          <div class="mdc-list-item__graphic material-icons">{button.icon}</div>
          <div class="mdc-list-item__text">{button.text}</div>
        </Item>
      {/each}
    </List>
  {/if}
</div>
