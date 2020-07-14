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
import Data.Maybe
import Data.Set as Set
import Data.Store
import Data.Time.Clock
import qualified Distribution.Hackage.DB.Parsed as P
import Distribution.Hackage.DB.Path
import qualified Distribution.Hackage.DB.Unparsed as U
import Distribution.PackageDescription.Parsec
import Distribution.PackageDescription.PrettyPrint
import Distribution.Parsec
import Distribution.Pretty
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
import System.FilePath.Posix

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

setLibDeps ::
  [Dependency] ->
  BuildInfo ->
  BuildInfo
setLibDeps deps binfo@BuildInfo {} = binfo {targetBuildDepends = deps}

getDeps :: GenericPackageDescription -> [Dependency]
getDeps GenericPackageDescription {..} = concat (maybeToList (fmap go condLibrary))
  where
    go (CondNode Library {..} _ _) = targetBuildDepends libBuildInfo

setDeps ::
  [Dependency] ->
  GenericPackageDescription ->
  GenericPackageDescription
setDeps deps pkg@GenericPackageDescription {..} = pkg {condLibrary = fmap go condLibrary}
  where
    go (CondNode var@Library {..} libdeps libs) =
      CondNode (var {libBuildInfo = setLibDeps deps libBuildInfo}) libdeps libs

majorVersion :: Version -> Version
majorVersion = alterVersion go
  where
    go [a, b, _, _] = [a, b]
    go [a, b, _] = [a, b]
    go [a, b] = [a, b]
    go [a] = [a]
    go [] = []
    go _ = error "Non-PVP versioning scheme in Hackage metadata. Should never happen."

add :: Dependency -> FilePath -> IO ()
add dep cabalFile =
  case depVerRange dep of
    AnyVersion -> do
      desc <- readGenericPackageDescription normal cabalFile
      let pk = depPkgName dep
      verMap <- cacheDeps
      ver <- case Map.lookup pk verMap of
        Nothing -> die $ "No such named: " ++ show pk
        Just vers -> pure (maximum vers)
      let dependency = Dependency pk (majorBoundVersion (majorVersion ver)) (Set.singleton defaultLibName)
      putStrLn $ "Adding latest dependency: " ++ prettyShow dependency ++ " to " ++ takeFileName cabalFile
      let desc' = addDep dependency desc
      writeGenericPackageDescription cabalFile desc'
    ThisVersion givenVersion -> addVer ThisVersion givenVersion cabalFile dep
    LaterVersion givenVersion -> addVer LaterVersion givenVersion cabalFile dep
    OrLaterVersion givenVersion -> addVer OrLaterVersion givenVersion cabalFile dep
    EarlierVersion givenVersion -> addVer EarlierVersion givenVersion cabalFile dep
    WildcardVersion givenVersion -> addVer WildcardVersion givenVersion cabalFile dep
    givenVersion -> die $ "Given version is not on available on Hackage." ++ show givenVersion

addVer :: (Version -> VersionRange) -> Version -> FilePath -> Dependency -> IO ()
addVer f givenVersion cabalFile dep = do
  desc <- readGenericPackageDescription normal cabalFile
  let pk = depPkgName dep
  verMap <- cacheDeps
  let dependency = Dependency pk (f givenVersion) (Set.singleton defaultLibName)
  let desc' = addDep dependency desc
  case Map.lookup pk verMap of
    Nothing -> die $ "No such named: " ++ show pk
    Just vers ->
      if givenVersion `elem` vers
        then do
          putStrLn $ "Adding explicit dependency: " ++ prettyShow dependency ++ " to " ++ takeFileName cabalFile
          writeGenericPackageDescription cabalFile desc'
        else die $ "Given version is not on available on Hackage." ++ show givenVersion

cacheDeps :: IO (Map PackageName [Version])
cacheDeps = do
  cache <- cacheDb
  cacheExists <- doesFileExist cache
  if cacheExists
    then do
      dbContents <- BS.readFile cache
      case decode dbContents of
        Left _ -> die "Corrupted cabal cache file. Run 'cabal-edit rebuild'."
        Right db -> pure db
    else do
      putStrLn "No cache file found, building from HackageDB."
      buildCache

buildCache :: IO (Map PackageName [Version])
buildCache = do
  cache <- cacheDb
  hdb <- hackageTarball
  now <- getCurrentTime
  tdb <- U.readTarball (Just now) hdb
  vers <- forM (Map.toList tdb) $ \(pk, pdata) -> do
    let verMap = Map.keys (P.parsePackageData pk pdata)
    pure (pk, verMap)
  let db = Map.fromList vers
  BS.writeFile cache (encode db)
  pure db

completerPacks :: IO [String]
completerPacks = do
  db <- cacheDeps
  pure (unPackageName <$> Map.keys db)

-------------------------------------------------------------------------------
-- Comamnds
-------------------------------------------------------------------------------

listCmd :: String -> IO ()
listCmd packName = do
  pk <- case (simpleParsec packName :: Maybe PackageName) of
    Nothing -> die "Invalid package name."
    Just pk -> pure pk
  verMap <- cacheDeps
  vers <- case Map.lookup pk verMap of
    Nothing -> die $ "No such named: " ++ show pk
    Just vers -> pure (sort vers)
  mapM_ (putStrLn . prettyShow) vers

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

rebuildCmd :: IO ()
rebuildCmd = buildCache >> putStrLn "Done."

-------------------------------------------------------------------------------
-- Orphan Sinbin
-------------------------------------------------------------------------------

instance Store PackageName

instance Store ShortText

instance Store Version

-------------------------------------------------------------------------------
-- Options Parsing
-------------------------------------------------------------------------------

cacheFile :: FilePath
cacheFile = ".cabal-cache.db"

cacheDb :: IO FilePath
cacheDb = do
  home <- getHomeDirectory
  cabalExists <- doesDirectoryExist (home </> ".cabal")
  if cabalExists
    then pure (home </> ".cabal" </> cacheFile)
    else die "No ~/.cabal directory found. Is cabal installed?"

data Cmd
  = Add String
  | List String
  | Upgrade String
  | Remove String
  | Rebuild
  deriving (Eq, Show)

addParse :: [String] -> Parser Cmd
addParse localPackages = Add <$> argument str (metavar "PACKAGE" <> completeWith localPackages)

listParse :: [String] -> Parser Cmd
listParse localPackages = List <$> argument str (metavar "PACKAGE" <> completeWith localPackages)

opts :: [String] -> Parser Cmd
opts localPackages =
  subparser $
    mconcat
      [ command "add" (info (addParse localPackages) (progDesc "Add dependency to cabal file.")),
        command "list" (info (listParse localPackages) (progDesc "List available versions from Hackage.")),
        command "rebuild" (info (pure Rebuild) (progDesc "Rebuild cache."))
      ]

main :: IO ()
main = do
  comps <- completerPacks
  let options = info (opts comps <**> helper) idm
  cmd <- customExecParser p options
  case cmd of
    Add dep -> addCmd dep
    List dep -> listCmd dep
    Rebuild -> rebuildCmd
    Upgrade _ -> pure ()
    Remove _ -> pure ()
  where
    p = prefs showHelpOnEmpty
