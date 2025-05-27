{
  # nixos
  config,
  pkgs,
  lib,
  # tempest
  nlib,
  lua,
  encode,
  # satellite
  mirosSnippetCache,
  obsidianVault,
  ...
}:
let
  # keybind = keybinds: { inherit keybinds; };
  autocmd = autocmds: { inherit autocmds; };
  void = lua "";
  ilua = code: lua code // { inline = true; };
  keymap = mode: mapping: action: desc: {
    inherit
      mode
      mapping
      action
      desc
      ;
  };
  nmap = keymap "n";
  unmap = mapping: {
    inherit mapping;
    action = "<nop>";
  };
in
[
  (<vim/loader/enable> void)
  # {{{ Base setup
  # {{{ General options
  {
    vim.g = {
      # Disable filetype.vim
      do_filetype_lua = true;
      did_load_filetypes = false;

      # Set leader
      mapleader = " ";
    };

    vim.opt = {
      # Basic options
      joinspaces = false; # No double spaces with join (mapped to qj in my config)
      list = false; # I don't want to show things like tabs
      cmdheight = 0; # Hide command line when it's not getting used
      spell = true; # Spell checker

      # tcqj are there by default, and "r" automatically continues comments on enter
      formatoptions = "tcqjr";

      scrolloff = 4; # Starts scrolling 4 lines from the edge of the screen
      termguicolors = true; # True color support

      wrap = false; # Disable line wrap (by default)
      wildmode = [
        "list"
        "longest"
      ]; # Command-line completion mode
      completeopt = [
        "menu"
        "menuone"
        "noselect"
      ];

      undofile = true; # persist undos!!

      # {{{ Line numbers
      number = true; # Show line numbers
      relativenumber = true; # Relative line numbers
      # }}}
      # {{{ Indents
      expandtab = true; # Use spaces for the tab char
      shiftwidth = 2; # Size of an indent
      tabstop = 2; # Size of tab character
      shiftround = true; # When using < or >, rounds to closest multiple of shiftwidth
      smartindent = true; # Insert indents automatically
      # }}}
      # {{{ Casing
      ignorecase = true; # Ignore case
      smartcase = true; # Do not ignore case with capitals
      # }}}
      # {{{ Splits
      splitbelow = true; # Put new windows below current
      splitright = true; # Put new windows right of current
      # }}}
      # {{{ Folding
      foldmethod = "marker"; # use {{{ }}} for folding
      foldcolumn = "0"; # show no column with folds on the left
      # }}}
    };
  }
  # }}}
  # {{{ Misc keybinds
  {
    # {{{ Global keybinds
    keys = [
      # {{{ Free up q and Q
      (nmap "<c-q>" "q" "Record macro")
      (nmap "<c-s-q>" "Q" "Repeat last recorded macro")
      (unmap "q")
      (unmap "Q")
      # }}}
      # {{{ Chords
      # Different chords get remapped to f-keys by my [my kaanta config](../../../hosts/nixos/common/optional/services/kanata.nix).
      #
      # Exit insert mode using *jk*
      (keymap "iv" "<f10>" "<esc>" "Exit insert mode")

      # Use global clipboard using *cp*
      (keymap "nv" "<f11>" ''"+'' "Use global clipboard")
      # Save using *ji*
      (nmap "<f12>" (_: [
        (<vim/cmd> "silent! write")
        { vim.opt.stl = <vim/opt/stl>; }
      ]) "Save current file")
      # }}}
      # {{{ Newline without comments
      {
        mode = "i";
        mapping = "<c-cr>";
        action =
          _:
          <vim/paste> [
            # Lines to paste
            [
              ""
              ""
            ]
            # Non-streaming paste
            (-1)
          ];
        desc = "Insert newline without continuing the current comment";
      }
      {
        mode = "i";
        mapping = "<c-s-cr>";
        # This is a bit scuffed and might not work for all languages
        action = "<cmd>norm O<bs><bs><bs><cr>";
        desc = "Insert newline above without continuing the current comment";
      }
      # }}}
      # {{{ Diagnostics
      (nmap "J" <vim/diagnostic/open_float> "Open current diagnostic")
      (nmap "<leader>D" <vim/diagnostic/setloclist> "[D]iagnostic loclist")
      (nmap "qj" "J" "join lines")
      # }}}
      # {{{ Other misc keybinds
      (nmap "<Leader>a" "<C-^>" "[A]lternate file")
      (unmap "<C-^>")
      (nmap "Q" ":wqa<cr>" "Save all files and [q]uit")
      (nmap "<leader>rw" ":%s/<C-r><C-w>/" "[R]eplace [w]ord in file")
      (nmap "<leader>sw" (<require> "my.helpers.wrap" /toggle) "toggle word [w]rap")
      (nmap "<leader>ss" { vim.opt.spell = ilua "not vim.o.spell"; } "toggle [s]pell checker")
      (nmap "<leader>yp" "<cmd>!curl --data-binary @% https://paste.rs<cr>" "[y]ank [p]aste.rs link")
      # }}}
    ];
    # }}}
    # {{{ Autocmds
    autocmds = [
      # {{{ Exit certain buffers with qq
      {
        event = "FileType";
        pattern = [ "help" ];
        group = "BasicBufferQuitting";
        action.keys = nmap "qq" "<cmd>close<cr>" "[q]uit current buffer";
      }
      # }}}
      # {{{ Enable wrap movemenets by default in certain filetypes
      {
        event = "FileType";
        pattern = [
          "markdown"
          "typst"
          "tex"
        ];
        group = "EnableWrapMovement";
        action = <require> "my.helpers.wrap" /enable;
      }
      # }}}
    ];
    # }}}
  }
  # }}}
  # {{{ Simple autocmds
  # {{{ Disable pseudo-transparency;
  (autocmd {
    event = "FileType";
    group = "WinblendSettings";
    action.vim.opt.winblend = 0;
  })
  # }}}
  # {{{ Starter page
  [
    (lua ''
      local header
      if vim.loop.cwd() == ${encode obsidianVault} then
        header = ${encode (builtins.readFile ./headers/obsidian.txt)}
      else
        header = ${encode (builtins.readFile ./headers/main.txt)}
      end
    '')
    (<require> "my.starter" /setup { header = <header>; })
  ]
  # }}}
  # {{{ Manage cmdheight
  (autocmd {
    event = "CmdlineEnter";
    group = "SetCmdheightCmdlineEnter";
    action.vim.opt.cmdheight = 1;
  })
  (autocmd {
    event = "CmdlineLeave";
    group = "SetCmdheightCmdlineLeave";
    action.vim.opt.cmdheight = 0;
  })
  # }}}
  # }}}
  # {{{ Lsp settings
  # {{{ Change lsp on-hover borders
  {
    vim.lsp.handlers."textDocument/hover" = <vim/lsp/with> [
      <vim/lsp/handlers/hover>
      { border = "single"; }
    ];
    vim.lsp.handlers."textDocument/signatureHelp" = <vim/lsp/with> [
      <vim/lsp/handlers/signature_help>
      { border = "single"; }
    ];
  }
  # }}}
  # {{{ Create on-attach keybinds
  (autocmd {
    event = "LspAttach";
    group = "UserLspConfig";
    action =
      let
        nmap =
          mapping: action: desc:
          nlib.nmap mapping <vim/lsp/buf> /${action} desc;
      in
      {
        context_ = event: {
          bufnr = event /buf;
          client = <vim/lsp/get_client_by_id> (event /data/client_id);
        };
        keys = [
          (nlib.nmap "<leader>li" "<cmd>LspInfo<cr>" "[L]sp [i]nfo")
          (nmap "gd" "definition" "[G]o to [d]efinition")
          (nmap "<leader>gi" "implementation" "[G]o to [i]mplementation")
          (nmap "<leader>gr" "references" "[G]o to [r]eferences")
          (nmap "L" "signature_help" "Signature help")
          (nmap "<leader>c" "code_action" "[C]ode actions")
          (keymap "v" "<leader>c" ":'<,'> lua vim.lsp.buf.range_code_action()" "[C]ode actions")
          (nmap "<leader>wa" "add_workspace_folder" "[W]orkspace [A]dd Folder")
          (nmap "<leader>wr" "remove_workspace_folder" "[W]orkspace [R]emove Folder")
          (nlib.nmap "<leader>wl" (
            _: <print> (<vim/inspect> (<vim/ps/buf/list_workspace_folders> void))
          ) "[W]orkspace [L]ist Folders")
        ];
        action = {
          cond = <context/client/supports_method> "textDocument/hover";
          keys = nmap "K" "hover" "Hover";
        };
      };
  })
  # }}}
  # }}}
  # {{{ Language/editor specific settings
  # {{{ Nix settings
  (autocmd {
    event = "FileType";
    group = "UserNixSettings";
    pattern = "nix";
    action = {
      # Set nix commentstring
      vim.opt.commentstring = "# %s";

      # Allow running `update-nix-fetchgit` with a single keypress
      keys = {
        desc = "Update all fetchgit calls";
        mapping = "<leader>lg";
        action =
          let
            update = lib.getExe pkgs.update-nix-fetchgit;
          in
          _: <tempest/withSavedCursor> (_: <vim/cmd> ":%!${update}");
      };
    };
  })
  # }}}
  # {{{ Purescript settings
  (autocmd {
    event = "FileType";
    group = "UserPurescriptSettings";
    pattern = "purs";
    action.vim.opt = {
      expandtab = true; # Use spaces for the tab char
      commentstring = "-- %s";
    };
  })
  # }}}
  # {{{ Neovide config
  {
    cond = <tempest/whitelist> "neovide";
    vim.g = {
      neovide_transparency = config.stylix.opacity.applications;
      neovide_cursor_animation_length = 4.0e-2;
      neovide_cursor_animate_in_insert_mode = false;
    };
  }
  # }}}
  # }}}
  # }}}
  # {{{ Set up lazy.nvim
  (<require> "lazy" /setup [
    # {{{ Load plugin sources
    [
      (<require> "my.plugins.themes")
      (<require> "my.plugins.whichkey")
      (<unpack> (<require> "nix" /lazy))
    ]
    # }}}
    # {{{ Configure lazy.nvim
    {
      # Lazy-load by default
      defaults.lazy = true;

      install = {
        # install missing plugins on startup. this doesn't increase startup time.
        missing = true;
        # try to load one of these colorschemes when starting an installation during startup
        colorscheme = [
          "catppuccin"
          "rose-pine"
        ];
      };

      # Don't live-update running instances of neovim
      change_detection.enabled = false;
      change_detection.notify = false;

      dev = {
        # Fallback to git when local plugin doesn't exist
        fallback = true;

        # Directory where I store my local plugin projects
        path = config.xdg;

        # When to look for local plugins
        patterns = [ "prescientmoon" ];
      };

      performance.rtp = {
        # Paths to add to runtimepath
        paths = [
          "${pkgs.vimPlugins.lazy-nvim}"
          mirosSnippetCache
        ];

        # Disable some built-in plugins for performance.
        disabled_plugins = [
          "gzip"
          "matchit"
          "matchparen"
          "netrwPlugin"
          "tarPlugin"
          "tohtml"
          "tutor"
          "zipPlugin"
        ];
      };
    }
    # }}}
  ])
  # }}}
]
