{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    { nixpkgs, self }:
    let
      hpkgs = pkgs: pkgs.haskell.packages."ghc9101";
      # nihil = pkgs: pkgs.haskell.packages."ghc9101".callPackage ./nihil.nix { };
      nihil =
        pkgs:
        (hpkgs pkgs).developPackage {
          root = ./.;
        };

      # shell =
      #   pkgs:
      #   (hpkgs pkgs).developPackage {
      #     root = ./.;
      #     modifier =
      #       drv:
      #       pkgs.haskell.lib.addBuildTools drv (
      #         with (hpkgs pkgs);
      #         [
      #           cabal-install
      #           ghcid
      #           fourmolu
      #           haskell-language-server
      #         ]
      #       );
      #   };
      shell =
        pkgs:
        pkgs.mkShell {
          packages = with (hpkgs pkgs); [
            cabal-install
            ghcid
            fourmolu
            haskell-language-server
          ];
        };
    in
    {
      packages."x86_64-linux".nihil = nihil nixpkgs.legacyPackages."x86_64-linux";
      packages."x86_64-linux".default = self.packages."x86_64-linux".nihil;
      devShells."x86_64-linux".nihil = shell nixpkgs.legacyPackages."x86_64-linux";
      devShells."x86_64-linux".default = self.devShells."x86_64-linux".nihil;
    };
}
