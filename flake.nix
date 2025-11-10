{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import "${nixpkgs}" {
          inherit system;
        };
        setupEnvironment = haskellPackages: ''
          export PYTHON_BIN_PATH=${pkgs.python3}/bin/python3
          export NIX_GHC=${haskellPackages.ghc.withPackages (p: (mkCradle haskellPackages).buildInputs)}/bin/ghc
          export NIX_GHCPKG=${haskellPackages.ghc.withPackages (p: (mkCradle haskellPackages).buildInputs)}/bin/ghc-pkg
          export NIX_GHC_LIBDIR=$($NIX_GHC --print-libdir)
        '';
        mkCradle = haskellPackages:
          pkgs.haskell.lib.overrideCabal
            (haskellPackages.callPackage ./cabal2nix.nix { })
            (old: {
              preBuild = setupEnvironment haskellPackages;
              buildDepends = (old.buildDepends or [ ]) ++ [ pkgs.just haskellPackages.doctest pkgs.cabal-install ];
              postCheck = ''
                HOME=$(pwd)
                just doctest
              '';
            });
      in
      rec {
        lib = {
          inherit mkCradle;
        };
        packages =
          (if system == "x86_64-linux"
          then {
            withGhc9_02 = lib.mkCradle pkgs.haskell.packages.ghc92;
          }
          else { })
          //
          {
            withGhc9_04 = lib.mkCradle pkgs.haskell.packages.ghc94;
            withGhc9_06 = lib.mkCradle pkgs.haskell.packages.ghc96;
            withGhc9_10 = lib.mkCradle pkgs.haskell.packages.ghc910;
            withGhc9_12 = lib.mkCradle pkgs.haskell.packages.ghc912;
            default = lib.mkCradle pkgs.haskellPackages;
          };
        checks = {
          hlint = pkgs.runCommand "hlint" { buildInputs = [ pkgs.hlint ]; }
            ''
              cd ${./.}
              hlint src test
              touch $out
            '';
        };
        devShells = {
          default = pkgs.mkShell {
            shellHook = setupEnvironment pkgs.haskellPackages;
            buildInputs = with pkgs; [
              cabal2nix
              ghcid
              ormolu
              cabal-install
              (pkgs.haskellPackages.ghc.withPackages (p: packages.default.buildInputs))
              packages.default.buildInputs
              (haskell-language-server.override { dynamic = true; })
              pkgs.haskellPackages.cabal2nix
              nixpkgs-fmt
              nil
              pkgs.haskellPackages.doctest
              just
              hlint
            ];
          };
        };
      }
    );
}
