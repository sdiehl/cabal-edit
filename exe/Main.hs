{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
  ( main,
  )
where

import Control.Monad
import qualified Data.ByteString as BS
import Data.List
import Data.Map as Map
import Data.Set as Set
import Data.Store
import Data.Time.Clock
import qualified Distribution.Hackage.DB.Parsed as P
import Distribution.Hackage.DB.Path
import qualified Distribution.Hackage.DB.Unparsed as U
import Distribution.PackageDescription.Parsec
import Distribution.PackageDescription.PrettyPrint
import Distribution.Parsec
import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.Dependency
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library
import Distribution.Types.LibraryName
import Distribution.Types.PackageName
import Distribution.Types.Version
import Distribution.Types.VersionRange.Internal
import Distribution.Utils.ShortText
import Distribution.Verbosity
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath.Glob

-------------------------------------------------------------------------------
-- Dependency Addition
-------------------------------------------------------------------------------

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
addLibDep dep binfo@BuildInfo {..} = binfo {targetBuildDepends = targetBuildDepends <> [dep]}

majorVersion :: Version -> Version
majorVersion = alterVersion go
  where
    go [a, b, _, _] = [a, b]
    go [a, b, _] = [a, b]
    go [a, b] = [a, b]
    go [a] = [a]
    go [] = []
    go _ = error "Non-PVP versioning scheme. Bad programmer, bad."

add :: Dependency -> FilePath -> IO ()
add dep cabalFile = do
  desc <- readGenericPackageDescription normal cabalFile
  let pk = depPkgName dep
  verMap <- cacheDeps
  ver <- case Map.lookup pk verMap of
    Nothing -> die $ "No such named: " ++ show pk
    Just vers -> pure (last (sort vers))
  let dependency = Dependency pk (majorBoundVersion (majorVersion ver)) (Set.singleton defaultLibName)
  putStrLn $ "Adding dependency: " ++ show ver
  --print dependency
  let desc' = addDep dependency desc
  --pPrint desc'
  writeGenericPackageDescription cabalFile desc'

cacheDb :: FilePath
cacheDb = ".cabal-cache.db"

cacheDeps :: IO (Map PackageName [Version])
cacheDeps = do
  cacheExists <- doesFileExist cacheDb
  if cacheExists
    then do
      dbContents <- BS.readFile cacheDb
      case decode dbContents of
        Left _ -> die "Corrupted cabal cache file."
        Right db -> pure db
    else do
      putStrLn "No cache file found, building from HackageDB."
      hdb <- hackageTarball
      now <- getCurrentTime
      tdb <- U.readTarball (Just now) hdb
      vers <- forM (Map.toList tdb) $ \(pk, pdata) -> do
        let verMap = Map.keys (P.parsePackageData pk pdata)
        pure (pk, verMap)
      let db = Map.fromList vers
      BS.writeFile cacheDb (encode db)
      pure db

completerPacks :: IO [String]
completerPacks = do
  db <- cacheDeps
  pure (fmap unPackageName $ Map.keys db)

instance Store PackageName

instance Store ShortText

instance Store Version

-------------------------------------------------------------------------------
-- Options Parsing
-------------------------------------------------------------------------------

data Cmd
  = Add String
  | List String
  | Upgrade String
  | Remove String
  deriving (Eq, Show)

addParse :: [String] -> Parser Cmd
addParse localPackages = Add <$> argument str (metavar "PACKAGE" <> completeWith localPackages)

listParse :: [String] -> Parser Cmd
listParse = List <$> argument str (metavar "PACKAGE" <> completeWith localPackages)

listCmd :: String -> IO ()
listCmd packName = do
  print packName
  pure ()

addCmd :: String -> IO ()
addCmd packName = do
  dep <- case (simpleParsec packName :: Maybe Dependency) of
    Nothing -> die "Invalid dependency version number."
    Just dep -> pure dep
  cabalFiles <- glob "*.cabal"
  case cabalFiles of
    [] -> die "No cabal file found in current directory."
    [fname] -> add dep fname
    _ -> die "Multiple cabal-files found."

opts :: [String] -> Parser Cmd
opts localPackages =
  subparser $
    mconcat
      [ command "add" (info (addParse localPackages) (progDesc "Add dependency to cabal file.")),
        command "list" (info (listParse localPackages) (progDesc "List available versions from Hackage."))
      ]

main :: IO ()
main = do
  comps <- completerPacks
  let options = info ((opts comps) <**> helper) idm
  cmd <- customExecParser p options
  case cmd of
    Add dep -> addCmd dep
    List dep -> listCmd dep
    Upgrade _ -> pure ()
    Remove _ -> pure ()
  where
    p = prefs showHelpOnEmpty
