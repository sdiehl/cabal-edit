{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List
import Data.Map as Map
import Data.Set as Set
import Data.Time.Clock
import qualified Distribution.Hackage.DB.Parsed as P
import Distribution.Hackage.DB.Path
import qualified Distribution.Hackage.DB.Unparsed as U
import Distribution.PackageDescription.Parsec
import Distribution.PackageDescription.PrettyPrint
import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.Dependency
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library
import Distribution.Types.LibraryName
import Distribution.Types.PackageName
import Distribution.Types.Version
import Distribution.Types.VersionRange.Internal
import Distribution.Verbosity
import System.Environment
import Text.Pretty.Simple

-- add
-- remove
-- upgrade

addDep ::
  Dependency ->
  GenericPackageDescription ->
  GenericPackageDescription
addDep dep pkg@GenericPackageDescription {..} = pkg {condLibrary = fmap go condLibrary}
  where
    go (CondNode var@Library {..} deps libs) =
      CondNode (var {libBuildInfo = addLibDep dep libBuildInfo}) (deps <> [dep]) libs

addLibDep ::
  Dependency ->
  BuildInfo ->
  BuildInfo
addLibDep dep info@BuildInfo {..} = info {targetBuildDepends = targetBuildDepends <> [dep]}

majorVersion :: Version -> Version
majorVersion = alterVersion go
  where
    go [a, b, c, d] = [a, b]
    go [a, b, c] = [a, b]
    go [a, b] = [a, b]
    go [a] = [a]
    go [] = []
    go _ = error "Non-PVP versioning scheme. Bad programmer, bad."

main :: IO ()
main = do
  let fname = "cabal-add.cabal"
  [packName] <- getArgs
  desc <- readGenericPackageDescription normal fname
  let pk = mkPackageName packName
  hdb <- hackageTarball
  now <- getCurrentTime
  putStrLn "Reading tarball"
  db <- U.readTarball (Just now) hdb
  pdata <- case Map.lookup pk db of
    Nothing -> error "No such package"
    Just pdata -> pure pdata
  putStrLn "Parsing package database"
  let verMap = P.parsePackageData pk pdata
  let ver = last (sort (Map.keys verMap))
  let dependency = Dependency pk (majorBoundVersion (majorVersion ver)) (Set.singleton defaultLibName)
  --print dependency
  let desc' = addDep dependency desc
  --pPrint desc'
  writeGenericPackageDescription fname desc'
