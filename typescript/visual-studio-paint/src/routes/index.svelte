<script>
  import Button from "@smui/button"

  let count = 0

  $: double = count * 2
</script>

<Button
  on:click={() => {
    count++
  }}>
  Increase
</Button>
Count: {count} Double: {double}
