list:
  just --list

ci: hpack fmt test nix-build

fmt: fmt-nix fmt-haskell

fmt-nix:
  nixpkgs-fmt *.nix

fmt-haskell:
  #!/usr/bin/env bash

  ormolu \
    --mode inplace \
    $(find . -name '*.hs' | grep -v '^./dist-newstyle/')

hpack:
  hpack

test: hpack
  cabal test --test-show-details=streaming

watch *args="": hpack
  ghcid --command "cabal repl test:spec" --test ':main {{ args }}' --warnings

nix-build:
  nix -L build
