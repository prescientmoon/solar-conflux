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
  <List>
    {#each buttons as button, index}
      <Item on:SMUI:action={button.action || (() => {})}>
        <Graphic class="material-icons">{button.icon}</Graphic>
        <Text>{button.text}</Text>
      </Item>
    {/each}
  </List>
</div>
