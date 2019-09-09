<script>
  import Sidebar from "../components/Sidebar.svelte"
  import Paint from "../components/Paint.svelte"

  import { renderingContext$ } from "../stores/renderingContext"
  import { sidebarButtons as buttons } from "../constants/sidebar.ts"
</script>

<style>
  :global(a) {
    user-select: none;
    -webkit-user-drag: none;
  }
</style>

<svelte:head>
  <link
    href="https://fonts.googleapis.com/icon?family=Material+Icons"
    rel="stylesheet" />
</svelte:head>

<Sidebar {buttons}>
  <div class="full" slot="content">
    <Paint {renderingContext$} height={1000} width={1000} />
  </div>
  <div slot="panel">
    <slot />
  </div>
</Sidebar>
