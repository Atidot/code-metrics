{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
module Code.Metrics.Main where

import           "base"       GHC.Generics
import           "base"       Control.Monad.IO.Class (MonadIO, liftIO)
import           "base"       Data.List (isSuffixOf, isInfixOf, sort, nub, intercalate, sortBy)
import           "base"       System.Environment (getArgs)
import qualified "bytestring" Data.ByteString.Char8 as B (pack)
import qualified "bytestring" Data.ByteString.Lazy.Char8 as BL (putStrLn)
import           "time"       Data.Time.Clock (UTCTime(..))
import           "time"       Data.Time.LocalTime (LocalTime(..), ZonedTime(..), utc, utcToLocalTime)
import           "time"       Data.Time.Calendar (fromGregorian)
import           "tagged"     Data.Tagged
import           "cassava"    Data.Csv
import           "dates"      Data.Dates
import           "Cabal"      Distribution.Verbosity (silent)
import           "Cabal"      Distribution.Types.Dependency
import           "Cabal"      Distribution.Types.PackageName
import           "Cabal"      Distribution.PackageDescription
import           "Cabal"      Distribution.PackageDescription.Parsec
import           "Cabal"      Distribution.Types.GenericPackageDescription
import           "uniplate"   Data.Generics.Uniplate.Data (universeBi)
import           "resourcet"  Control.Monad.Trans.Resource (runResourceT)
import           "conduit"    Data.Conduit
import qualified "conduit"    Data.Conduit.List as CL
import qualified "conduit"    Data.Conduit.Combinators as CC
import           "gitlib"     Git.Types
import           "gitlib"     Git.Repository
import           "gitlib"     Git.Reference
import           "gitlib-libgit2" Git.Libgit2 (lgFactory, LgRepo)
--import           "gitlib-cmdline" Git.CmdLine (lgFactory, LgRepo)


--
data Language
    = Haskell
    | Python
    | Nix
    | Javascript
    | R
    | C
    | Dhall
    | Prolog
    | Cabal
    | JupyterNotebook
    | HTML
    | CSS
    | Makefile
    | Cue
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

languages :: [Language]
languages = [toEnum 0 .. ]

instance ToField Language where
    toField lang = B.pack . show $ lang

extension :: Language -> String
extension Haskell         = ".hs"
extension Python          = ".py"
extension Nix             = ".nix"
extension Javascript      = ".js"
extension R               = ".r"
extension C               = ".c"
extension Dhall           = ".dhall"
extension Prolog          = ".pl"
extension Cabal           = ".cabal"
extension JupyterNotebook = ".ipynb"
extension HTML            = ".html"
extension CSS             = ".css"
extension Makefile        = "Makefile"
extension Cue             = ".cue"

--
data Status
    = Status
    { status_language     :: !Language
    , status_extension    :: !String
    , status_loc          :: !Int
    , status_packages     :: !Int
    , status_date         :: !DateTime
    , status_dependencies :: ![String]
    } deriving (Show, Eq, Generic, ToNamedRecord, DefaultOrdered)

instance ToField [String] where
    toField strings
        = B.pack
        . intercalate " "
        $ strings

instance ToField DateTime where
    toField dt = B.pack . show . year $ dt

--
isLanguage :: Language -> FilePath -> Bool
isLanguage language path = extension language `isSuffixOf` path


filterLanguageC :: (Monad m) => Language -> Conduit FilePath m FilePath
filterLanguageC Haskell  = CC.filter (isLanguage Haskell) .| CC.filter (not . isInfixOf ".stack-work")
filterLanguageC language = CC.filter (isLanguage language)


linesCountC :: (Monad m, MonadIO m) => Conduit FilePath m Int
linesCountC = CC.mapM
            $ fmap length
            . fmap lines
            . liftIO
            . readFile


filesC = CC.sourceDirectoryDeep False

countLines :: Language -> FilePath -> IO Int
countLines language path
    = runResourceT
    . runConduit
    $ filesC path
   .| filterLanguageC language
   .| linesCountC
   .| CC.sum


--
countPackages :: Language -> FilePath -> IO Int
countPackages language
    = case language of
        Haskell -> countPackages Cabal
        Cabal   -> run (isLanguage Cabal)
        Python  -> run (== "setup.py")
        _       -> (\_ -> return 0)
    where
        run pattern path
            = runResourceT
            . runConduit
            $ filesC path
           .| CC.filter pattern
           .| CC.length

--
cabalBuildDeps :: String -> IO [String]
cabalBuildDeps path = do
    cabal <- readGenericPackageDescription silent path
    let packages = universeBi :: (Maybe (CondTree ConfVar [Dependency] Library) -> [PackageName])
    return . map unPackageName
           . packages
           . condLibrary
           $ cabal


haskellDependencies :: FilePath -> IO [String]
haskellDependencies path
    = nub . sort . mconcat <$> depss
    where
        depss :: IO [[String]]
        depss = runResourceT
              . runConduit
              $ filesC path
             .| filterLanguageC Cabal
             .| CC.mapM (liftIO . cabalBuildDeps)
             .| CL.consume


dependencies :: Language -> FilePath -> IO [String]
dependencies Haskell = haskellDependencies
dependencies _       = (\_ -> return [])


--
commits :: FilePath -> IO [Commit LgRepo]
commits path = do
    withRepository lgFactory path $ do
        (Just ref) <- resolveReference "HEAD"
        let t = renderOid ref
        o <- parseObjOid t
        runConduit $ sourceObjects Nothing o False
                  .| CC.map (\(CommitObjOid x) -> x)
                  .| CC.mapM lookupCommit
                  .| CL.consume


sortByWhen :: [Commit LgRepo] -> [Commit LgRepo]
sortByWhen = sortBy compareWhen
    where
        compareWhen :: Commit LgRepo -> Commit LgRepo -> Ordering
        compareWhen (Commit _ _ _ (Signature _ _ (ZonedTime when1 _)) _ _ _)
                    (Commit _ _ _ (Signature _ _ (ZonedTime when2 _)) _ _ _)
            | when1 > when2  = LT
            | when1 == when2 = EQ
            | when1 < when2  = GT

commitsBefore :: UTCTime -> [Commit LgRepo] -> [Commit LgRepo]
commitsBefore date
    = sortByWhen
    . filter older
    where
        localTime :: LocalTime
        localTime = utcToLocalTime utc date

        older (Commit _ _ _(Signature _ _ (ZonedTime when _)) _ _ _) = when < localTime

lastCommitBefore :: UTCTime -> [Commit LgRepo] -> Maybe (Commit LgRepo)
lastCommitBefore date commits
    | null commits' = Nothing
    | otherwise     = Just $ head commits'
    where
        commits' :: [Commit LgRepo]
        commits' = commitsBefore date commits

date :: (Integer, Int) -> UTCTime
date (year, month) = UTCTime (fromGregorian year month 1) 0

--
status :: Language -> FilePath -> IO Status
status language path = do
    locs         <- countLines language path
    packages     <- countPackages language path
    dependencies <- dependencies language path
    today        <- getCurrentDateTime
    return $ Status
           { status_language     = language
           , status_extension    = extension language
           , status_loc          = locs
           , status_packages     = packages
           , status_dependencies = dependencies
           , status_date         = today
           }

--
main = do
    (path:_) <- getArgs
    commits <- commits path
    let mCommit = lastCommitBefore (date (2020, 3)) commits
    print $ commitOid <$> mCommit
    return ()

        ----runConduit $ sourceObjects Nothing (parseObjOid "a0282c1b276366b6e76ad82b77f23d1898702310") False .| CC.print
        --return ()
    ----statuses <- mapM (flip status path) languages
    ----BL.putStrLn $ encodeDefaultOrderedByName statuses
    --return ()
