{ nixpkgs ? import ./nixpkgs.nix {}, compiler ? "ghc864" }:

(import ./default.nix { inherit nixpkgs compiler; }).env
