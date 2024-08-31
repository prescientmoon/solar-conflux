{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          pyDeps = p: with p; [ numpy scipy matplotlib pillow jupyter ];
        in
        rec {
          devShell = pkgs.mkShell {
            packages = with pkgs; [
              (python3.withPackages pyDeps)
              pkgs.texlive.combined.scheme-full
              # pandoc
            ];
          };
        });
}
