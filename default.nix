let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.callPackage ./cabal-edit.nix { }
