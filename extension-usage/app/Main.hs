{-# LANGUAGE DeriveGeneric #-}

module Main where

import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library (libBuildInfo)
import Distribution.Types.BuildInfo (BuildInfo(defaultExtensions))
import Distribution.Verbosity
import Language.Haskell.Extension

import Data.List (sortBy)
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
               , usedExtensions :: [String]
               , releaseDate :: UTCTime
               }
    deriving (Generic)
instance ToJSON Pkg

main :: IO ()
main = do
    defExts <- getDefaultExts
    extPrags <- getExtPrags
    let usedExts :: M.Map Package (S.Set Extension)
        usedExts = M.unionWith (<>) defExts extPrags

    writeFile "pkg-exts" $ unlines
        [ unwords $ [pkg, show $ S.size exts] ++ [ show ext | EnableExtension ext <- S.toList exts ]
        | (pkg, exts) <- M.toList usedExts
        ]

    let hist :: M.Map KnownExtension Int
        hist = M.fromListWith (+)
            [ (ext, 1)
            | exts <- M.elems usedExts
            , EnableExtension ext <- S.toList exts
            ]
    writeFile "ext-hist" $ unlines
        [ unwords [show ext, show n]
        | (ext, n) <- sortBy (comparing snd) (M.toList hist)
        ]

    cabalFiles <- glob "recent-trees/**/*.cabal"
    releaseDates <- M.fromList <$> sequence
        [ do mtime <- getModificationTime cabalFile
             return (pkg, mtime)
        | cabalFile <- cabalFiles
        , let pkg = splitDirectories cabalFile !! 1
        ]

    Data.Aeson.encodeFile "packages.json"
        [ Pkg { name = pkg
              , usedExtensions = [ show ext | EnableExtension ext <- S.toList exts ]
              , releaseDate = mtime
              }
        | (pkg, exts) <- M.toList usedExts
        , let Just mtime = pkg `M.lookup` releaseDates 
        ]

type Package = String

getExtPrags :: IO (M.Map Package (S.Set Extension))
getExtPrags = do
    srcFiles <- G.glob "recent-trees/**/*.hs"
    M.fromListWith (<>) <$> sequence
        [ do exts <- getModuleExts src
             return (pkg, S.fromList exts)
        | src <- srcFiles
        , let pkg = splitDirectories src !! 1
        ]

getDefaultExts :: IO (M.Map Package (S.Set Extension))
getDefaultExts = do
    cabalFiles <- G.glob "recent-trees/*/*.cabal"
    M.fromList <$> mapM getPkgDefaultExts cabalFiles

getPkgDefaultExts :: FilePath -> IO (String, S.Set Extension)
getPkgDefaultExts cabalFile = do
    pd <- readGenericPackageDescription silent cabalFile
    let defExts :: [Extension]
        defExts = maybe [] (foldMap (defaultExtensions . libBuildInfo)) (condLibrary pd)
        pkg = splitDirectories cabalFile !! 1
    return (pkg, S.fromList defExts)

