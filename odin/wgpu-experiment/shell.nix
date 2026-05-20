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
    # Odin tooling
    pkgs.odin # Compiler

    # Generic tooling
    pkgs.pkg-config
    pkgs.mold # Linker
    pkgs.just # Script runner
    pkgs.samply # Profiler
    pkgs.gdb # Debugger
    pkgs.seer # Debugger GUI
    pkgs.valgrind # Detect memory leaks
    pkgs.wgpu-utils
  ];

  buildInputs = [
    pkgs.wgpu-native
    pkgs.glfw
  ];

  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
