{ pkgs ? import ./nixpkgs.nix {}
, compiler ? "ghc865"
}:

pkgs.haskell.packages."${compiler}".callCabal2nix "cabal-edit" ./. {}
