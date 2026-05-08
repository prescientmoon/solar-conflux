{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs:
    {
      overlays.default = final: prev: { };
    }
    // inputs.flake-utils.lib.eachSystem (with inputs.flake-utils.lib.system; [ x86_64-linux ]) (
      system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system}.extend inputs.self.overlays.default;
        inherit (pkgs) lib;
      in
      {
        # {{{ Shell
        devShell = pkgs.mkShell rec {
          nativeBuildInputs = [
            pkgs.odin # Compiler
            pkgs.mold # Linker
            pkgs.just # Script runner
            pkgs.samply # Profiler
            pkgs.ols # Language server
            pkgs.gdb # Debugger
            pkgs.seer # Debugger GUI
            pkgs.valgrind # Detect memory leaks
          ];

          buildInputs = [ ];

          LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath buildInputs;
        };
        # }}}
      }
    );
}
