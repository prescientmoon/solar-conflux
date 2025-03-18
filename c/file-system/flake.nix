{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs:
    inputs.flake-utils.lib.eachSystem (with inputs.flake-utils.lib.system; [ x86_64-linux ]) (
      system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
      in
      {
        # {{{ Shell
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.gnumake
            pkgs.gcc
            pkgs.valgrind
            pkgs.seer # Debugger
            pkgs.delta # Diff pager
          ];
        };
        # }}}
      }
    );
}
