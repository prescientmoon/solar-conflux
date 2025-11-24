let
  sources = import ./npins;
in
{
  pkgs ? import sources.nixpkgs { },
}:
pkgs.mkShell rec {
  nativeBuildInputs = [
    pkgs.pkg-config
    pkgs.mold # Linker
    pkgs.just # Script runner
    pkgs.samply # Profiler
    pkgs.gdb # Debugger
    pkgs.seer # Debugger GUI
    pkgs.valgrind # Detect memory leaks
    pkgs.npins # Nix pinning

    # Rust tooling
    pkgs.rustc
    pkgs.cargo
    pkgs.rustfmt
    pkgs.clippy
    pkgs.rust-analyzer
  ];

  buildInputs = [ ];
  LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath buildInputs;
}
