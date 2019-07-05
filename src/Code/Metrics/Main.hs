{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
module Code.Metrics.Main where

import           "base"       GHC.Generics
import           "base"       Control.Monad.IO.Class (MonadIO, liftIO)
import           "base"       Data.List (isSuffixOf, isInfixOf, sort, nub, intercalate)
import           "base"       System.Environment (getArgs)
import qualified "bytestring" Data.ByteString.Char8 as B (pack)
import qualified "bytestring" Data.ByteString.Lazy.Char8 as BL (putStrLn)
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
    statuses <- mapM (flip status path) languages
    BL.putStrLn $ encodeDefaultOrderedByName statuses
    return ()
