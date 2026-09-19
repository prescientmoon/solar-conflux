{
  description = "iios";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix";
  };

  outputs = { self, nixpkgs, easy-purescript-nix, ... }@inputs:
    let
      name = "iios";

      supportedSystems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in
    {
      devShell = forAllSystems (system:
        let
          pkgs = import nixpkgs { 
            inherit system;
          };
          easy-ps = import easy-purescript-nix { inherit pkgs; };
        in
        pkgs.mkShell {
          inherit name;
          buildInputs = (with pkgs; [
            # nodejs-16_x
            # Not really using the nodejs-mobile-react-native so far.
            nodejs-18_x
            nodePackages.yarn
            nixpkgs-fmt
            esbuild
          ]) ++ (with easy-ps; [
            purs
            purs-tidy
            psa
            spago
            purescript-language-server
            purs-backend-es
          ]) ++ (pkgs.lib.optionals (system == "aarch64-darwin") (
            (with pkgs; [
              cocoapods
              fastlane
            ]) ++ (with pkgs.darwin.apple_sdk.frameworks; [
              Cocoa
              CoreServices
              WebKit
              Security
            ]))
          );

        });
    };
}
