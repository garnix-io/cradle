{
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
        '';
        src = ./.;
        mkCradle = haskellPackages:
          pkgs.haskell.lib.overrideCabal
            (haskellPackages.callCabal2nix "cradle" src { })
            (old: {
              preBuild = setupEnvironment haskellPackages;
              buildDepends = (old.buildDepends or [ ]) ++ [ pkgs.just haskellPackages.doctest ];
              postCheck = ''
                just doctest
              '';
            });
      in
      rec {
        lib = {
          inherit mkCradle;
        };
        packages = {
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
