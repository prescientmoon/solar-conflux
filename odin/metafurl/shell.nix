let
  sources = import ./npins;
in
{
  pkgs ? import sources.nixpkgs { },
}:
pkgs.mkShell {
  nativeBuildInputs = [
    # Odin tooling
    pkgs.odin # Compiler
    pkgs.ols # Language server

    # Generic tooling
    pkgs.pkg-config
    pkgs.mold # Linker
    pkgs.just # Script runner
    pkgs.samply # Profiler
    pkgs.gdb # Debugger
    pkgs.seer # Debugger GUI
    pkgs.valgrind # Detect memory leaks
  ];
}
