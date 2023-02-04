{-# LANGUAGE DeriveGeneric #-}

module Main where

import Distribution.Text
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.PackageDescription (PackageDescription(package))
import Distribution.Types.Library as Cabal (libBuildInfo)
import Distribution.Types.PackageId (PackageIdentifier(..))
import qualified Distribution.Types.BuildInfo as Cabal (BuildInfo(defaultExtensions))
import Distribution.Verbosity
import Language.Haskell.Extension

import Control.Exception
import Control.Concurrent.Async.Pool
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.FilePath.Glob as G
import System.FilePath
import GHC.Generics
import Data.Time.Clock (UTCTime)
import System.Directory

import Data.Aeson

import LangPrag

data Pkg = Pkg { name :: String
               , version :: String
               , defaultExtensions :: S.Set Extension
               , extensionPragmas :: S.Set Extension
               , releaseDate :: UTCTime
               }
    deriving (Show, Generic)
instance ToJSON Pkg
instance FromJSON Pkg

usedExtensions :: Pkg -> S.Set Extension
usedExtensions pkg = defaultExtensions pkg <> extensionPragmas pkg

instance FromJSON Extension where
    parseJSON v = do
        s <- parseJSON v
        case simpleParse s of
          Just ext -> return ext
          Nothing  -> fail $ "Failed to parse extension " <> s

instance ToJSON Extension where
    toJSON = toJSON . display

main :: IO ()
main = do
    pkgs <- readPackages "recent-trees"
    writeFile "pkg-exts" $ unlines
        [ unwords $ [name pkg, show $ S.size exts] ++ [ display ext | ext <- S.toList exts ]
        | pkg <- pkgs
        , let exts = usedExtensions pkg
        ]

    let hist :: M.Map Extension Int
        hist = M.fromListWith (+)
            [ (ext, 1)
            | pkg <- pkgs
            , ext <- S.toList $ usedExtensions pkg
            ]
    writeFile "ext-hist" $ unlines
        [ unwords [show ext, show n]
        | (ext, n) <- sortBy (comparing snd) (M.toList hist)
        ]

    Data.Aeson.encodeFile "packages.json" pkgs

readPackages :: FilePath -> IO [Pkg]
readPackages dir = do
    pkgs <- listDirectory dir
    let mkPkg :: FilePath -> IO (Maybe Pkg)
        mkPkg pkg =
            handle onErr $ fmap Just $ mkPackage $ dir </> pkg
          where
            onErr :: SomeException -> IO (Maybe Pkg)
            onErr e = Nothing <$ print (pkg, e)

    withTaskGroup 8 $ \tg -> do
        catMaybes <$> mapTasks tg (map mkPkg pkgs)

mkPackage :: FilePath -> IO Pkg
mkPackage dir = do
    cabalFiles <- glob $ dir </> "*.cabal"
    cabalFile <- case cabalFiles of
                   [] -> fail $ "no cabal file found in " <> dir
                   [f] -> return f
                   _ -> fail $ "too many cabal files found in " <> dir
    pd <- readGenericPackageDescription silent cabalFile
    let defExts :: [Extension]
        defExts = maybe [] (foldMap (Cabal.defaultExtensions . Cabal.libBuildInfo)) (condLibrary pd)

    let pid@(PackageIdentifier name ver) = package $ packageDescription pd
    let tarball = "tarballs" </> display name </> display pid <.> "tar.gz"
    mtime <- getModificationTime tarball

    srcFiles <- G.glob $ dir </> "**/*.hs"
    extPrags <- mconcat <$> sequence
        [ do exts <- getModuleExts src
             return $ S.fromList exts
        | src <- srcFiles
        ]

    return $ Pkg { name = display name
                 , version = display ver
                 , releaseDate = mtime
                 , defaultExtensions = S.fromList defExts
                 , extensionPragmas = extPrags
                 }
