module LangPrag (getModuleExts) where

import Language.Haskell.Extension
import Distribution.Text

import Text.Regex.TDFA.Text as RE
import Text.Regex.TDFA.Common as RE
import Text.Regex.Base as RE

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS

import Control.Monad (unless)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)

langPragRegexp :: Regex
langPragRegexp =
    -- N.B. https://github.com/haskell-hvr/regex-tdfa/issues/11
    let Right re = RE.compile copts eopts (T.pack "{-#[[:space:]]*LANGUAGE[[:space:]]+([^#]+)#-}")
        copts = RE.defaultCompOpt { caseSensitive = False, multiline = False }
        eopts = RE.ExecOption { captureGroups = True }
    in re

decodeHsSource :: BS.ByteString -> T.Text
decodeHsSource src = do
    case TE.decodeUtf8' src of
      Left _ -> TE.decodeLatin1 src
      Right t -> t

getModuleExts :: FilePath -> IO [Extension]
getModuleExts modPath = do
    exts <- parseModuleExts . decodeHsSource <$> BS.readFile modPath
    let (unknown, exts') = partition isUnknownExt exts
        isUnknownExt (UnknownExtension _) = True
        isUnknownExt _ = False
    unless (null unknown) $ putStrLn $ "Unknown extensions " ++ show unknown
    return exts'

parseModuleExts :: T.Text -> [Extension]
parseModuleExts src =
    let ms = RE.matchAllText langPragRegexp src

        extNames :: [T.Text]
        extNames = 
            [ T.strip ext
            | groups <- ms
            , [_,group] <- pure $ toList groups
            , (src, (_offset,_len)) <- pure group
            , ext <- T.split (==',') src
            ]
     in map parseExtension extNames

parseExtension :: T.Text -> Extension
parseExtension t =
    fromMaybe (UnknownExtension s) (simpleParse s)
  where s = T.unpack t
