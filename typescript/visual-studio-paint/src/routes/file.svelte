<script>
  import List from "../components/PanelButtonList"
  import { filePanelButtonConfigs as buttons } from "../constants/filePanel"
  import { handleImageUpolad } from "../helpers/handleImageUpolad"
  import { getContext } from "svelte"

  let dropping = false

  const painting = getContext("painting")

  const drop = e => {
    dropping = false

    const files = e.dataTransfer.files

    for (const file of files) {
      if (file.type.startsWith("image/")) {
        return handleImageUpolad(painting, file)
      }
    }
  }

  const setDropping = value => () => {
    dropping = value
  }
</script>

<style>
  #upolad {
    padding: var(--upolad-padding);
    margin: var(--upolad-margin);
    border: var(--panel-text-color) 1px solid;

    background-color: var(--bg);
  }

  #upolad-text {
    text-align: center;
  }

  #upolad.dropping {
    filter: brightness(1.3);
  }

  #upolad-description {
    width: 100%;
    text-align: center;
  }
</style>

<div class="header">File:</div>

<List {buttons} />

<div
  id="upolad"
  class:dropping
  on:dragover|preventDefault
  on:drop|preventDefault={drop}
  on:dragenter={setDropping(true)}
  on:dragleave={setDropping(false)}>
  <div id="upolad-text">Upolad</div>
</div>

<div id="upolad-description">Drop your file onto the "upolad" zone</div>
