{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import "${nixpkgs}" {
          inherit system;
        };
        ourHaskell = pkgs.haskell.packages.ghc945;
        setupEnvironment = ''
          export BASH_PATH=${pkgs.bash}/bin/bash
        '';
        src = ./.;
        cradle =
          pkgs.haskell.lib.overrideCabal
            (ourHaskell.callCabal2nix "cradle" src { })
            (_: { preBuild = setupEnvironment; });
      in
      {
        packages.default = cradle;
        devShells = {
          default = pkgs.mkShell {
            shellHook = setupEnvironment;
            buildInputs = with pkgs; [
              ghcid
              ormolu
              cabal-install
              (ourHaskell.ghc.withPackages (p: cradle.buildInputs))
              cradle.buildInputs
              (haskell-language-server.override {
                dynamic = true;
                supportedGhcVersions = [ "945" ];
              })
              ourHaskell.cabal2nix
              nixpkgs-fmt
              nil
            ];
          };
        };
      }
    );
}
