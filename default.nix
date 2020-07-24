{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc864"
}:

pkgs.haskell.packages."${compiler}".callCabal2nix "cabal-edit" ./. {}
