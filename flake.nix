{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import "${nixpkgs}" {
          inherit system;
        };
        mkHaskell = ghcVersion: pkgs.haskell.packages.${"ghc${ghcVersion}"};
        setupEnvironment = ''
          export BASH_PATH=${pkgs.bash}/bin/bash
        '';
        src = ./.;
        mkCradle = ghcVersion:
          let haskell = mkHaskell ghcVersion;
          in pkgs.haskell.lib.overrideCabal
            (haskell.callCabal2nix "cradle" src { })
            (old: {
              preBuild = setupEnvironment;
              buildDepends = (old.buildDepends or [ ]) ++ [ pkgs.just haskell.doctest ];
              postCheck = ''
                just doctest
              '';
            });
        devShell = ghcVersion:
          let
            haskell = mkHaskell ghcVersion;
            cradle = mkCradle ghcVersion;
          in
          pkgs.mkShell
            {
              shellHook = setupEnvironment;
              buildInputs = with pkgs; [
                ghcid
                ormolu
                cabal-install
                (haskell.ghc.withPackages (p: cradle.buildInputs))
                cradle.buildInputs
                (haskell-language-server.override {
                  dynamic = true;
                  supportedGhcVersions = [ ghcVersion ];
                })
                haskell.cabal2nix
                nixpkgs-fmt
                nil
                haskell.doctest
                just
              ];
            };
      in
      {
        packages = rec {
          default = ghc945;
          ghc8107 = mkCradle "8107";
          ghc945 = mkCradle "945";
        };
        devShells = rec {
          default = ghc945;
          ghc8107 = devShell "8107";
          ghc945 = devShell "945";
        };
      }
    );
}
