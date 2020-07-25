{ pkgs ? import ./nixpkgs.nix {}
, compiler ? "ghc865"
}:

let
  src = pkgs.nix-gitignore.gitignoreSource [ "/.git/" "/*.nix" ] ./.;

in
  pkgs.haskell.packages."${compiler}".callCabal2nix "cabal-edit" src {}
