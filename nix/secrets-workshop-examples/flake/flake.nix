{
  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixpkgs-unstable/nixexprs.tar.zst";
    starlitpkgs.url = "github:starlitcanopy/nixpkgs";
  };

  outputs =
    inputs:
    let
      forAllSystems = inputs.nixpkgs.lib.genAttrs [ "x86_64-linux" ];
    in
    {
      devShells = forAllSystems (
        system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          nixos-secrets = inputs.starlitpkgs.legacyPackages.${system}.nixos-secrets;
        in
        {
          example = pkgs.mkShell {
            packages = [ nixos-secrets ];
          };
        }
      );

      nixosConfigurations.example = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules =
          let
            base = "${inputs.starlitpkgs}/nixos/modules/security/secrets";
          in
          [
            base
            "${base}/example/common/backend-plain.nix"
            "${base}/example/common/backend-age.nix"
            "${base}/example/common/backend-prompt-simple.nix"
            ../config.nix
          ];
      };
    };
}
