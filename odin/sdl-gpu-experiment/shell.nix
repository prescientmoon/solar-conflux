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
    pkgs.wgpu-utils # Shader compilation
    pkgs.sdl3-shadercross # Even more shader compilation
    pkgs.jq # JSON handling
    pkgs.vulkan-tools
    pkgs.shader-slang
    pkgs.renderdoc
  ];

  buildInputs = [
    pkgs.sdl3
    pkgs.sdl3-image
    pkgs.libx11
    pkgs.libxcursor
    pkgs.libxext
    pkgs.libxfixes
    pkgs.libxi
    pkgs.libxrandr
    pkgs.wayland
    pkgs.vulkan-loader
    pkgs.vulkan-headers
    pkgs.vulkan-validation-layers
  ];

  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
