{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          pyDeps = p: with p; [ numpy scipy matplotlib ];
        in
        rec {
          devShell = pkgs.mkShell {
            buildInputs = with pkgs; [
              (python3.withPackages pyDeps)
              jupyter
              pandoc
            ];
          };
        });
}
