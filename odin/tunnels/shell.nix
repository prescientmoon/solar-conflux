{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell rec {
  nativeBuildInputs = [
    pkgs.odin
    pkgs.mold
  ];

  buildInputs = [
    pkgs.raylib
  ];

  LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath buildInputs;
}
