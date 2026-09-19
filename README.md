# The _solar conflux_

This repository contains numerous experiments of mine which do not deserve their own repositories. It exists for archival purposes.

The repositories are properly imported using [./import.sh](./import.sh) — a dangerous script that shouldn't be run unless everything in the local repo has been backed up.

## Navigation

The experiments are currently organized based on the language they use:

- [Bash](./bash/)
- [Dart](./dart/)
- [Elm](./elm/)
- [F#](./fsharp/)
- [Haskell](./haskell/)
- [Idris](./idris/)
- [Javascript](./javascript/)
- [Lean](./lean/)
- [Lua](./lua/)
- [Nix](./nix/)
- [Odin](./odin/)
- [Purescript](./purescript/)
- [Python](./python/)
- [Rust](./rust/)
- [Typescript](./typescript/)
- [Typst](./typst/)

## Philosophy

I plan to expand this repository to include all my projects, and flatten its
file structure (the per-language categorization is a bit odd).

Here's the basic laws:

### History is not sacred

- I might rewrite history at any point
- do _not_ reference stuff in this repo by commit hash!
- referencing things by date is the best idea (or tags perhaps, although I'm yet
  to figure that part out)

### Discovery is not a goal of the file structure

- trying to create deep hierarchies for the files in such a repo is a recipe for
  failure
- when trying to learn about the projects, one should instead check the page
  I'll create on my website (or the linked markup in case the website is down)

### The past is murky, but we can enforce conventions on the future

- For example, I'm not going to worry too much about the commit messages of
  the past (nor the formatting of the code), but I might enforce such
  conventions for the future
