let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.callCabal2nix "cabal-edit" ./. { }
