let
  sources = import ./npins;
in
{
  pkgs ? import sources.nixpkgs { },
}:

let
  nixos-secrets = (import sources.starlitpkgs { }).nixos-secrets;
in
pkgs.mkShell {
  packages = [
    pkgs.npins
    nixos-secrets
  ];
}
