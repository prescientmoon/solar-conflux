{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    odin.url = "github:starlitcanopy/odin";
    odin.inputs.nixpkgs.follows = "nixpkgs";
    odin.inputs.flake-utils.follows = "flake-utils";

    ols.url = "github:starlitcanopy/ols";
    ols.inputs.nixpkgs.follows = "nixpkgs";
    ols.inputs.flake-utils.follows = "flake-utils";
    ols.inputs.odin.follows = "odin";

    glsl_analyzer.url = "github:starlitcanopy/glsl_analyzer";
    glsl_analyzer.flake = false;
  };

  outputs =
    inputs:
    inputs.flake-utils.lib.eachSystem (with inputs.flake-utils.lib.system; [ x86_64-linux ]) (
      system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            inputs.odin.overlays.default
            inputs.ols.overlays.default
          ];
        };

        glsl_analyzer = pkgs.glsl_analyzer.overrideAttrs (_: {
          src = inputs.glsl_analyzer;

          nativeBuildInputs = [
            pkgs.zig_0_14.hook
          ];
        });

        inherit (pkgs) lib;
      in
      {
        # {{{ Shell
        devShell = pkgs.mkShell rec {
          nativeBuildInputs = [
            # Odin tooling
            pkgs.odin # Compiler
            pkgs.ols # Language server

            # Generic tooling
            pkgs.pkg-config
            pkgs.mold # Linker
            pkgs.just # Script runner
            pkgs.samply # Profiler
            pkgs.gdb # Debugger
            pkgs.seer # Debugger GUI
            pkgs.valgrind # Detect memory leaks

            # Python tooling
            pkgs.python3 # For script running
            pkgs.ruff # Python formatter

            # Rust tooling
            pkgs.rustc
            pkgs.cargo
            pkgs.rustfmt
            pkgs.clippy
            pkgs.rust-analyzer

            # Shader/GPU tooling
            glsl_analyzer # GLSL language server
            pkgs.renderdoc # Graphics debugger
            pkgs.glslang # GLSL linter / compiler
          ];

          buildInputs = [
            pkgs.sdl3
            pkgs.xorg.libX11
            pkgs.xorg.libXScrnSaver
            pkgs.xorg.libXcursor
            pkgs.xorg.libXext
            pkgs.xorg.libXfixes
            pkgs.xorg.libXi
            pkgs.xorg.libXrandr
          ];

          LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath buildInputs;
        };
        # }}}
      }
    );
}
