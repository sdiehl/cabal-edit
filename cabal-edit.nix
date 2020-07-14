{ mkDerivation, base, bytestring, Cabal, containers, directory
, stdenv
}:
mkDerivation {
  pname = "cabal-edit";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring Cabal containers directory filepath Glob hackage-db
    optparse-applicative store time
  ];
  testHaskellDepends = [];
  homepage = "https://github.com/sdiehl/cabal-edit";
  description = "A utility for managing Hackage dependencies from the command line.";
  license = stdenv.lib.licenses.mit;
}
