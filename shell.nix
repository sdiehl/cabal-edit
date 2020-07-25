{ pkgs ? import ./nixpkgs.nix {}
, compiler ? "ghc865"
}:

(import ./default.nix { inherit pkgs compiler; }).env
