{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callCabal2nix "cabal-edit" ./. { }
