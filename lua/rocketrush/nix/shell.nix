{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = [
    pkgs.love
    pkgs.zip
    (pkgs.python3Packages.callPackage ./love-web-builder.nix { })
  ];
}
