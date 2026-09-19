#import "@preview/polylux:0.3.1": *
#import themes.simple: *

// Make the paper dimensions fit for a presentation and the text larger
#set page(paper: "presentation-16-9")
#set text(size: 25pt)
#set text(font: "New Computer Modern")
#show raw: set text(font: "Cascadia Mono", size: 20pt)

#show heading: it => [
  #set text(weight: "black", font: "Cascadia Code")
  #block(it.body)
]

#let custom_icon = (path, kind:"svg") => box(baseline:6pt)[#image("assets/icons/"+{path}+"." + {kind}, width: 1em)]
#let github = custom_icon("github")
#let haskell = custom_icon("haskell")
#let latex = custom_icon("latex")
#let lua = custom_icon("lua")
#let nix = custom_icon("nix")
#let rust = custom_icon("rust")
#let tempest = custom_icon("tornado")
#let cake = custom_icon("cake")
#let vim = custom_icon("vim")
#let neovim = box(baseline:7pt)[#image("assets/neovim.png", height: 1em)]
#let github-link = path => [
  #github
  #link("https://github.com/" + path)[#raw(path)]
]


#focus-slide(background: rgb("#5300fa"))[
  #align(horizon + center)[
    = Creating the #tempest tempest
    #set text(font: "Cascadia Mono", size: 20pt, fill: rgb("#cccccc"))
    a cursed new take on declarative Neovim configuration
  ]
]

#polylux-slide[
  #align(horizon)[
    = Who am I?  #custom_icon("lagrange", kind: "png")
    - Math student #pause
    - I love #nix
    - I use #neovim for everything:
      - note taking + homework (#latex)
      - my job (#haskell programming)
      - personal projects (#nix, #rust, etc)
  ]
]

#polylux-slide[
  #align(horizon)[
    #side-by-side[
    = What is #neovim ?

    - terminal text editor
    - very extensible
    - #vim Vimscript or #lua Lua
    ][
      #image("assets/neovim-screenshot-cat.png")
    ]
  ]
]

#polylux-slide[
 = Why configure #neovim using #nix nix?

  #align(horizon)[
  1. Access to all the options already defined using nix
  2. Collocation of plugins and their nix dependencies
    #pause
    #show raw: set text(size: 15pt)
    ```nix
    rust-tools = {
      package = "simrat39/rust-tools.nvim";
      dependencies.nix = [
        pkgs.rust-analyzer
        pkgs.rustfmt
      ];
    };
    ```
  ]
]

#polylux-slide[
  = Existing approaches #custom_icon("cozyluna", kind: "png")

  #align(horizon)[
  1. Install plugins using #neovim + configure using #lua lua
  - #nix ```nix config.lib.file.mkOutOfStoreSymlink```
  #pause
  2. Install plugins using #nix nix + configure using #lua lua
  - built-in #nix nixos module
  - #github-link("BirdeeHub/nixCats-nvim")
  #pause
  3. Try to do as much as possible using #nix
  - #github-link("nix-community/nixvim")
  ]
]

#focus-slide(background: rgb("#F5A9B8"))[
  = Can we have our cake and eat it too?
  // #custom_icon("gracestare", kind: "jpg")
]

#polylux-slide[
  #show raw: set text(size: 30pt)
  = #github-link("prescientmoon/tempest") (soon)

  #align(horizon)[
  - DSL that allows writing #lua lua as #nix nix #pause
  - Can be incrementally used next to traditional configs
  - Plugins can be installed in any way
  - Can even be used for non-#neovim #(lua)-based apps
  ]
]

// #polylux-slide[
//   == #nix An example
//   #align(center + horizon)[
//   #show raw: set text(size: 15pt)
//   ```nix
//   (autocmd {
//     group = "UserNixSettings";
//     event = "FileType"; # Event listener that runs...
//     pattern = "nix"; # ...when detecting a nix file
//     action = {
//       vim.opt.commentstring = "# %s"; # Set nix comment syntax
//       keys = { # Allow running `update-nix-fetchgit` with a series of keypresses
//         mapping = "<leader>lg";
//         action = # Run `update-nix-fetchgit` over the entire file...
//           let update = lib.getExe pkgs.update-nix-fetchgit;
//           in # ...saving the current cursor position
//           _: <tempest/withSavedCursor> (_: <vim/cmd> ":%!${update}");
//       };
//     };
//   })
//   ```
//   ]
// ]

// #polylux-slide[
//   = #nix => #lua (0) — raw code
//
//   #align(horizon)[
//     #show math.equation: set text(font: "Cascadia Mono")
//     - Introduce a function $"lua" :: "string" -> #lua$.
//     - Example usage:
//       ```nix
//       obsidian = {
//         package = "epwalsh/obsidian.nvim";
//         cond =
//           lua "vim.loop.cwd() == /home/moon/projects/vault";
//       };
//       ```
//   ]
// ]
#polylux-slide[
  #align(horizon+center)[
  == (a note on examples)
  ]
]

#polylux-slide[
  = #nix => #lua (0) — primitives

  #align(horizon)[
  - Think of ```nix builtins.toJSON``` & friends #pause
  - Example usage:
    #show raw: set text(size: 15pt)
    ```nix
    lastplace = {
      package = "ethanholz/nvim-lastplace";
      opts.lastplace_ignore_buftype = [
        "quickfix" "nofile" "help"
      ];
    };
    ```
  ]
]

#polylux-slide[
  = #nix => #lua (1-2) — a challenge #custom_icon("angrytai", kind: "png")

  #align(horizon+center)[
    ```lua
    {
      "hrsh7th/nvim-cmp",
      dependencies = { ... },
      config = require("satellite.cmp"),
    }
    ```
  ]
]

#polylux-slide[
  = #nix => #lua (1) — variables

  #align(horizon)[
  1. ```nix <nixpkgs>``` is syntax sugar for
    ```nix
    builtins.findFile builtins.nixPath "nixpkgs"
    ```
  #pause

  2. ```nix builtins.something``` is always syntax sugar for ```nix __something```
  #pause

  3. We can shadow the builtin ```nix __findFile``` so that ```nix <require>``` in #nix translates to ```lua require``` in #lua
  ]
]

#polylux-slide[
  = #nix => #lua (2) — function application
  #align(horizon)[
  #set text(size: 20pt)
  #show raw: set text(size: 15pt)
  1. Normally, treating objects like functions make #nix sad
    ```nix
    { } "hello"
  # ^ attempt to call something which is not a function but a set
    ```
  #pause

  2. If the object has a property called ```nix __functor```, then #nix will call that instead:
    ```nix
    { __functor = self: hello: "${hello} world!"; } "hello"
    # hello world!
     ```
  #pause

  3. Hooray, we can make our #lua objects callable!
    - ```nix <require> "satellite.cmp"``` $=>$ ```lua require("satellite.cmp")```
    - ```nix <print> [ "foo" <bar> ]``` $=>$ ```lua print("foo", bar)```
  ]
]

#polylux-slide[
  = #nix => #lua (3): the dot — a new foe #custom_icon("hikarishock", kind: "png")
  #align(horizon+center)[
  ```lua
  {
    "hrsh7th/nvim-cmp",
    dependencies = { ... },
    config = require("satellite.cmp").config,
    init = require("satellite.cmp").init,
  }
  ```
  ]
]

#polylux-slide[
  = #nix => #lua (3) — property access
  #align(horizon)[
  - #nix has path types, which we do not yet use
  #pause
  - Solution: allow calling #lua objects with paths as arguments to generate property access
    - ```nix <require> "satellite.cmp" /config```
     \ $=>$ ```lua require("satellite.cmp").config```
  #pause
  - For convenience, allow embedding this inside our `<var>` syntax:
    - ```nix <vim/inspect> { hello = "world"; } ```
      \ $=>$ ```lua vim.inspect({ hello = "world" })```
  ]
]

#polylux-slide[
  = #nix => #lua (4) — the final boss? #custom_icon("canyou", kind: "png")
  #align(horizon+center)[
  ```lua
  {
    "hrsh7th/nvim-cmp",
    dependencies = { ... },
    init = function()
      vim.keymap.set(...)
    end
  }
  ```
  ]
]

#polylux-slide[
  = #nix => #lua (4) — functions
  #align(horizon)[
  #set text(size: 20pt)
  #show raw: set text(size: 15pt)
  - no general way to encode #nix functions as #lua functions #pause
  - generate argument name $=>$ pass it to the given function $=>$ expect a #lua object #pause
    - ```nix (_: <vim/keymap/set> [...])```
      \ $=>$ ```lua function() vim.keymap.set(...) end ``` #pause
    - ```nix (who: [ "hello" who ])```
      \ $=>$ ```lua function(a) return { "hello", a } end ```
  ]
]
#polylux-slide[
  = #nix => #lua (5) — blocks
  #align(horizon)[
  - we don't always want to return from functions
  - we might want to execute more than one action in a sequence
  ]
]
#polylux-slide[
  = #nix => #lua (5) — blocks
  #align(horizon+center)[
  Introducing, the `block` helper!

    #box(baseline: 45%, ```nix
    who: block [
      (<print> "hello")
      (<print> who)
    ]
    ```) #h(1.5cm) $=>$ #h(1.5cm)
    #box(baseline:40%, ```lua
    function(a)
      print("hello")
      print(a)
    end
    ```)
  ]
]

#polylux-slide[
  = #nix => #lua (6) — mutation
  #align(horizon)[
  #show raw: set text(size: 15pt)
    #pause
  - objects found inside blocks are treated as variable assignments:
    ```nix
    block {
      vim.opt = {
        wrap = false;
        undofile = true;
      };
    }
    ```
    turns into
    ```lua
    vim.opt.wrap = false
    vim.opt.undofile = true
    ```
  ]
]

#polylux-slide[
  = #nix => #neovim (0) — keybinds
  #align(horizon)[
  #show raw: set text(size: 25pt)
  We are allowed to create keybinds while inside a block:
  ```nix
  block {
    keys = {
      mode = "iv";
      mapping = "<f10>";
      action = "<esc>";
      desc = "Exit insert mode";
    };
  }
  ```
  // turns into
  // ```lua
  // vim.keymap.set(
  //   { "i", "v" },
  //   "<f10>",
  //   "<esc>",
  //   { desc = "Exit insert mode" }
  // )
  // ```
  ]
]

#polylux-slide[
  = #nix => #neovim (1) — autocmds
  #align(horizon)[
  #show raw: set text(size: 15pt)

  Autocmds are #vim's event listeners. #pause
  ```nix
  block {
    autocmds = {
      group = "UserNixSettings";
      event = "FileType";
      pattern = "nix";
      action = _: <print> "^~^ Nix file detected ^~^";
    };
  }
  ```
  ]
]

#polylux-slide[
  = #nix => #neovim (2) — context
  #align(horizon)[

  ```nix
  block {
    autocmds = {
      group = "UserNixSettings";
      event = "FileType";
      pattern = "nix";
      action.keys = {
        mapping = "<leader>lg";
        action = ...;
      };
    };
  }
  ```
  ]
]

// #polylux-slide[
//   = #nix => #neovim (3) — lazy.nvim
//   #align(horizon)[
//   #tempest provides helpers for using #github-link("folke/lazy.nvim")
//   #pause
//   #show raw: set text(size: 15pt)
//     ```nix
//     miros = {
//       dir = miros-nvim;
//       dependencies.nix = [ miros ];
//       ft = "miros";
//       keys = {
//         mapping = "...";
//         action = "..."
//       };
//     };
//     ```
//   ]
// ]

#polylux-slide[
  = Final thoughts
  #align(horizon)[
    - We now have a general yet concise way to write #lua in #nix #pause
    - Does it work? #pause
    - Should you use #tempest?
      // \ _if you can_ (\*singularity starts playing\*)
  ]
]
