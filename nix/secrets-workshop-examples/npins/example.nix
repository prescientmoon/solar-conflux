let
  sources = import ./npins;
in
import "${sources.nixpkgs}/nixos/lib/eval-config.nix" {
  modules =
    let
      base = "${sources.starlitpkgs}/nixos/modules/security/secrets";
    in
    [
      base
      "${base}/example/common/backend-plain.nix"
      "${base}/example/common/backend-age.nix"
      "${base}/example/common/backend-prompt-simple.nix"
      ../config.nix
    ];
}
