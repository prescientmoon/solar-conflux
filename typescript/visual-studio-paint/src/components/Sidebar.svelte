<script>
  import { stores } from "@sapper/app"
  import { fly, slide } from "svelte/transition"
  import { PersistentSubject } from "rxjs-extra"
  import { BehaviorSubject } from "rxjs"

  const { page } = stores()

  let panel = process.browser
    ? new PersistentSubject("panel", false)
    : new BehaviorSubject(false)

  const handleIconClick = e => {
    const parent = e.target.parentNode

    if (parent.href === location.href) {
      e.preventDefault()
      panel.next(!panel.value)
    }
  }

  export let buttons = []
</script>

<style>
  div#page {
    display: flex;
    height: 100vh;
    overflow: hidden;

    background-color: var(--bg);
  }

  nav#sidebar {
    display: flex;
    flex-direction: column;

    border-right: 1px solid var(--sidebar-divider-bg);

    padding: var(--sidebar-spacing);
    background-color: var(--sidebar-bg);
  }

  .sidebar-icon {
    color: var(--sidebar-text-color);
    margin: var(--sidebar-spacing);
    cursor: pointer;

    transition: filter var(--sidebar-transition);
    transition: text-shadow var(--sidebar-transition);
  }

  i.material-icons {
    font-size: 2em;
  }

  .sidebar-icon.current {
    filter: brightness(2);
    text-shadow: 0 0 var(--sidebar-glow-size) var(--sidebar-glow-color);
  }

  div#page-content {
    width: 100%;
    display: flex;
  }

  div#content {
    color: var(--text-color);
  }

  div#panel {
    width: var(--panel-width);
    height: 100%;
    box-shadow: border-box;

    padding-left: var(--panel-horizontal-padding);
    padding-right: var(--panel-horizontal-padding);
    padding-top: var(--panel-vertical-padding);
    padding-bottom: var(--panel-vertical-padding);

    color: var(--sidebar-text-color);
    background-color: var(--sidebar-bg);
  }
</style>

<div id="page">
  <nav id="sidebar">
    {#each buttons as button}
      <a
        class:sidebar-icon={true}
        class:current={$page.path === button.route}
        on:click={handleIconClick}
        href={button.route}>

        <i class="material-icons">{button.icon}</i>
      </a>
    {/each}
  </nav>
  <div id="page-content">
    {#if $panel}
      <aside>
        <div id="panel" transition:fly={{ x: -100, y: 0, duration: 100 }}>
          <slot name="panel">Panel content goes here</slot>
        </div>
      </aside>
    {/if}

    <div id="content" style={`width:calc(100% - var(--panel-width));`}>
      <main>
        <slot name="content" />
      </main>
    </div>
  </div>

</div>
