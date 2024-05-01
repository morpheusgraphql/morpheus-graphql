{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI.File where

import Data.ByteString.Lazy.Char8 (ByteString, readFile)
import Data.Char
import Data.Text (pack)
import Relude hiding (ByteString, readFile, writeFile)
import System.FilePath.Posix
  ( dropExtensions,
    makeRelative,
    normalise,
    replaceExtensions,
    splitDirectories,
    splitFileName,
    (</>),
  )

processFileName :: FilePath -> FilePath
processFileName = (\(x, y) -> x </> replaceExtensions (capitalize y) "hs") . splitFileName . normalise

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs

lookupFile :: FilePath -> IO (Maybe ByteString)
lookupFile x = fmap Just (readFile x) <|> pure Nothing

checkGenerated :: FilePath -> Maybe ByteString -> IO Bool
checkGenerated path result = do
  file <- lookupFile path
  let isOutdated = file /= result
  pure $ not isOutdated

getModuleNameByPath :: FilePath -> FilePath -> Text
getModuleNameByPath root path = pack . intercalate "." $ splitDirectories $ dropExtensions $ makeRelative root path
