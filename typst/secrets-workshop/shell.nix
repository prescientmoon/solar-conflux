let
  sources = import ./npins;
in
{
  pkgs ? import sources.nixpkgs { },
}:
pkgs.mkShell {
  packages = [
    pkgs.typst
    pkgs.tinymist
    pkgs.pdfpc
  ];
}
