{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc864"
}:

(import ./default.nix { inherit pkgs compiler; }).env
