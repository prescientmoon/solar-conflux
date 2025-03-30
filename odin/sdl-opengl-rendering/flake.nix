{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs:
    {
      overlays.default = final: prev: {
        # {{{ Odin
        odin = prev.odin.overrideAttrs (_: {
          version = "unstable-2025-03-28";
          src = final.fetchFromGitHub {
            owner = "starlitcanopy";
            repo = "Odin";
            rev = "a1fc243f8df8b510e6de4f5e115fbfc09371cb9d";
            sha256 = "0fb8lk47nb7ln0skjn3lyfi499q3wlnzp6w3qc4wf4s5zj43d6zh";
          };
        });
        # }}}
        # {{{ OLS
        ols = prev.ols.overrideAttrs (_: {
          version = "unstable-2025-03-12";
          src = final.fetchFromGitHub {
            owner = "DanielGavin";
            repo = "ols";
            rev = "1e44e3d78ad8a74ef09c7f54a6f6d3f7df517f8e";
            sha256 = "16f7b8ijcaj5m2bdgbbl1q1mzgpgzzazrap2g17hkgy63aqq8qmf";
          };

          installPhase = ''
            runHook preInstall

            install -Dm755 ols odinfmt -t $out/bin/
            cp      -r     builtin        $out/bin/
            wrapProgram $out/bin/ols --set-default ODIN_ROOT ${final.odin}/share

            runHook postInstall
          '';
        });
        # }}}
      };
    }
    // inputs.flake-utils.lib.eachSystem (with inputs.flake-utils.lib.system; [ x86_64-linux ]) (
      system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system}.extend inputs.self.overlays.default;
        raylib = pkgs.raylib.override { platform = "SDL"; };
        inherit (pkgs) lib;
      in
      {
        packages = { inherit (pkgs) odin; };

        # {{{ Shell
        devShell = pkgs.mkShell rec {
          nativeBuildInputs = [
            pkgs.pkg-config
            pkgs.entr # File change detection
            pkgs.odin # Compiler
            pkgs.mold # Linker
            pkgs.just # Script runner
            pkgs.samply # Profiler
            pkgs.ols # Language server
            pkgs.gdb # Debugger
            pkgs.seer # Debugger GUI
            pkgs.valgrind # Detect memory leaks
          ];

          buildInputs = [
            # pkgs.libGL
            # pkgs.libxkbcommon
            # pkgs.xorg.libXi
            # pkgs.xorg.libX11
            # pkgs.xorg.libXrandr
            # pkgs.xorg.libXinerama
            # pkgs.xorg.libXcursor
            # pkgs.wayland
            pkgs.sdl3
          ];

          LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath buildInputs;
        };
        # }}}
      }
    );
}
