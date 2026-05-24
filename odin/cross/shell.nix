let
  sources = import ./npins;
in
{
  pkgs ? import sources.nixpkgs { },
}:
let
  inherit (pkgs) lib;
in
pkgs.mkShell rec {
  nativeBuildInputs = [
    pkgs.odin # Compiler
    pkgs.zig # For cross compilation
    pkgs.pkg-config
    pkgs.mold # Linker
    pkgs.just # Script runner
    pkgs.valgrind # Detect memory leaks
    pkgs.xwin
  ];

  buildInputs = [
    pkgs.raylib
  ];

  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
