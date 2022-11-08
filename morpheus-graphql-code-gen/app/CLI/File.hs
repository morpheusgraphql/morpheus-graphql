{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI.File where

import Data.ByteString.Lazy.Char8 (ByteString, readFile, writeFile)
import Data.Char
import Data.Morpheus.Error (printError, printWarning)
import Data.Morpheus.Internal.Ext
  ( GQLResult,
    Result (..),
  )
import Data.Morpheus.Types.Internal.AST (GQLError (..), msg)
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

printWarnings :: [GQLError] -> IO ()
printWarnings [] = pure ()
printWarnings warnings = traverse_ (putStr . ("    " <>) . printWarning) warnings

cliError :: GQLError -> IO ()
cliError = putStr . ("    " <>) . printError "error" "\x1b[31m"

lookupFile :: FilePath -> IO (Maybe ByteString)
lookupFile x = fmap Just (readFile x) <|> pure Nothing

checkGenerated :: FilePath -> Maybe ByteString -> IO Bool
checkGenerated path result = do
  file <- lookupFile path
  let isOutdated = file /= result
  traverse_ cliError ["outdated: " <> msg path | isOutdated]
  pure $ not isOutdated

processDocument :: Bool -> FilePath -> GQLResult (Maybe ByteString) -> IO Bool
processDocument _ _ (Failure errors) = traverse_ cliError (toList errors) $> False
processDocument check path Success {result, warnings}
  | check = printWarnings warnings >> checkGenerated path result
  | otherwise = printWarnings warnings >> maybe (pure ()) (writeFile path) result $> True

getModuleNameByPath :: FilePath -> FilePath -> Text
getModuleNameByPath root path = pack . intercalate "." $ splitDirectories $ dropExtensions $ makeRelative root path
