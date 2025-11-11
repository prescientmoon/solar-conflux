{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  packages = [
    pkgs.ruff
    (pkgs.python313.withPackages (
      ps: with ps; [
        tabulate
        numpy
      ]
    ))
  ];
}
