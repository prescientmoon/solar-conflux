<script>
  import List, { Item, Graphic, Text, Separator } from "@smui/list"
  import { createBrushSelectionHandler } from "../helpers/selectBrush"

  export let currentBrush$
  export let brushes = []
</script>

<style>
  .container {
    --mdc-theme-text-primary-on-background: var(--panel-text-color);
    --mdc-theme-text-icon-on-background: var(--panel-icon-color);
  }

  div#brushes-header {
    font-size: 2rem;
    font-family: "Righteous", cursive;
  }
</style>

<div class="container">
  <div id="brushes-header">Brushes:</div>

  <List>
    {#each brushes as brush}
      <Item
        on:SMUI:action={createBrushSelectionHandler(currentBrush$, brush)}
        selected={$currentBrush$ === brush}>
        <Graphic class="material-icons">{brush.icon}</Graphic>
        <Text>{brush.name}</Text>
      </Item>
    {/each}
  </List>

</div>
