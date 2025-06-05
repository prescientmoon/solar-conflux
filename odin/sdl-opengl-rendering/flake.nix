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
            pkgs.pkg-config
            pkgs.odin # Compiler
            pkgs.mold # Linker
            pkgs.ols # Language server
            pkgs.just # Script runner
            pkgs.samply # Profiler
            pkgs.gdb # Debugger
            pkgs.seer # Debugger GUI
            pkgs.valgrind # Detect memory leaks
            pkgs.renderdoc # Graphics debugger
            glsl_analyzer # GLSL language server
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
