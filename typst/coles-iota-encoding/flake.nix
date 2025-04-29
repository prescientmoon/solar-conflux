{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    typix = {
      url = "github:loqusion/typix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:
    inputs.flake-utils.lib.eachSystem (with inputs.flake-utils.lib.system; [ x86_64-linux ]) (
      system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        typixLib = inputs.typix.lib.${system};
        lib = pkgs.lib;

        src = lib.fileset.toSource {
          root = ./.;
          fileset = lib.fileset.unions [
            ./images
            ./iotas.typ
          ];
        };

        commonArgs = handout: {
          typstSource = "iotas.typ";
          virtualPaths = [ ];

          fontPaths = [
            "${pkgs.cascadia-code}/share/fonts/truetype"
            "${pkgs.cascadia-code}/share/fonts/opentype"
            "${pkgs.cm_unicode}/share/fonts/opentype"
            "${pkgs.unifont}/share/fonts/opentype"
          ];

          unstable_typstPackages = [
            {
              name = "polylux";
              version = "0.4.0";
              hash = "sha256-4owP2KiyaaASS1YZ0Hs58k6UEVAqsRR3YdGF26ikosk=";
            }
          ];

          typstOpts.format = "pdf";
          typstOpts.input = "HANDOUT=${if handout then "true" else "false"}";
        };

        buildPresentation = handout: typixLib.buildTypstProject ((commonArgs handout) // { inherit src; });

        presentation = pkgs.linkFarm "coles-presentation" [
          {
            path = buildPresentation false;
            name = "slides.pdf";
          }
          {
            path = buildPresentation true;
            name = "handout.pdf";
          }
        ];
      in
      {
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.typst
            pkgs.pdfpc
          ];
        };

        packages.default = presentation;
      }
    );
}
