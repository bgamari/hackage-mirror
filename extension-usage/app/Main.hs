module Main where

import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library (libBuildInfo)
import Distribution.Types.BuildInfo (BuildInfo(defaultExtensions))
import Distribution.Verbosity
import Language.Haskell.Extension
import qualified Data.Map.Strict as M
import System.FilePath.Glob as G
import System.FilePath

main :: IO ()
main = do
    cabalFiles <- G.glob "recent-trees/*/*.cabal"
    defExts <- M.fromList <$> mapM getDefaultExts cabalFiles
    putStrLn $ unlines
        [ unwords $ [pkg] ++ map show exts
        | (pkg, exts) <- M.toList defExts
        ]

getDefaultExts :: FilePath -> IO (String, [Extension])
getDefaultExts cabalFile = do
    pd <- readGenericPackageDescription silent cabalFile
    let defExts :: [Extension]
        defExts = maybe [] (foldMap (defaultExtensions . libBuildInfo)) (condLibrary pd)
        pkg = splitDirectories cabalFile !! 1
    return (pkg, defExts)
