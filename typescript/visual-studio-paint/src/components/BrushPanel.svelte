<script>
  import Icon from "./Icon"
  import Option from "./Option"
  import List, { Item, Separator } from "@smui/list"
  import { createBrushSelectionHandler } from "../helpers/selectBrush"
  import { getContext } from "svelte"
  import { firstCharUppercase } from "../helpers/firstCharUppercase"

  const painting = getContext("painting")
  const { currentBrush$ } = painting
</script>

<style>
  .container {
    --mdc-theme-text-primary-on-background: var(--panel-text-color);
    --mdc-theme-text-icon-on-background: var(--panel-icon-color);
  }

  .option {
    width: 100%;

    padding: var(--brush-option-spacing);
    box-sizing: border-box;
  }

  #options {
    display: flex;
    flex-direction: column;
  }
</style>

<div class="container">
  <div class="header">Brushes:</div>

  {#if process.browser}
    <List>
      {#each painting.brushes as brush}
        <Item
          on:SMUI:action={createBrushSelectionHandler(currentBrush$, brush)}
          selected={$currentBrush$ === brush}>
          <div class="mdc-list-item__graphic">
            <Icon config={brush.icon} />
          </div>
          <div class="mdc-list-item__text">
            {firstCharUppercase(brush.name)}
          </div>
        </Item>
      {/each}
    </List>
  {/if}

  {#if process.browser && Object.values($currentBrush$.options || {}).length}
    <div class="header">Options:</div>

    <div id="options">
      {#each Object.entries($currentBrush$.options) as [name, option] (`${name} ${$currentBrush$.name} ${option.type}`)}
        {#if !option.hidden}
          <div class="option">
            <Option {name} {...option} />
          </div>
        {/if}
      {/each}
    </div>
  {/if}
</div>
