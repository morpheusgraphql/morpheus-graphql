{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Stack.Cabal
  ( checkCabal,
  )
where

import qualified Data.ByteString.Char8 as BS (unpack)
import Data.Map (lookup)
import qualified Data.Text as T
import GHC.IO.Exception (ExitCode (..))
import HConf.Config.ConfigT (ConfigT)
import HConf.Core.Version (Version)
import HConf.Utils.Class (HConfIO (..), Parse (..))
import HConf.Utils.Core (Name)
import HConf.Utils.Log (alert, field, subTask, task, warn)
import Relude
import System.Process

toLines :: Text -> [Text]
toLines = T.split (== '\n')

parseFields :: ByteString -> Map Text Text
parseFields =
  fromList
    . filter (not . T.null . fst)
    . map (bimap T.strip T.strip . (second (T.drop 1) . T.breakOn ":") . T.strip)
    . toLines
    . T.pack
    . BS.unpack

getField :: (MonadFail m) => Name -> Map Name a -> m a
getField k = maybe (fail $ "missing field" <> T.unpack k) pure . lookup k

getCabalFields :: FilePath -> Name -> ConfigT (Name, Version)
getCabalFields path pkgName = do
  bs <- read (path <> "/" <> T.unpack pkgName <> ".cabal")
  let fields = parseFields bs
  name <- getField "name" fields
  version <- getField "version" fields >>= parseText
  field (T.unpack name) (show version)
  pure (name, version)

noNewLine :: Char -> String
noNewLine '\n' = "          \n"
noNewLine x = [x]

stack :: String -> String -> [String] -> ConfigT ()
stack l name options = do
  (code, _, out) <- liftIO (readProcessWithExitCode "stack" (l : (name : map ("--" <>) options)) "")
  case code of
    ExitFailure {} -> alert (l <> ": " <> concatMap noNewLine (T.unpack $ T.strip $ T.pack out))
    ExitSuccess {} -> printWarnings l (parseWarnings out)

printWarnings :: String -> [(Text, [Text])] -> ConfigT ()
printWarnings name [] = field name "ok"
printWarnings name xs = task (T.pack name) $ traverse_ subWarn xs
  where
    subWarn (x, ls) =
      warn (T.unpack x)
        >> traverse_ (warn . T.unpack) ls

parseWarnings :: String -> [(Text, [Text])]
parseWarnings = concatMap toWarning . groupTopics . toLines . T.pack

groupTopics :: [Text] -> [[Text]]
groupTopics = regroup . break emptyLine
  where
    emptyLine = (== "")
    regroup (h, t)
      | null t = [h]
      | otherwise = h : groupTopics (dropWhile emptyLine t)

toWarning :: [Text] -> [(Text, [Text])]
toWarning (x : xs)
  | T.isPrefixOf "warning" (T.toLower x) = [(x, takeWhile (\p -> T.head p == ' ') xs)]
toWarning _ = []

buildCabal :: String -> ConfigT ()
buildCabal name = do
  stack "build" name ["test", "dry-run"]
  stack "sdist" name []

checkCabal :: Name -> Name -> Version -> ConfigT ()
checkCabal path name version = subTask "cabal" $ do
  buildCabal (T.unpack path)
  (pkgName, pkgVersion) <- getCabalFields (T.unpack path) name
  if pkgVersion == version && pkgName == name then pure () else fail (T.unpack path <> "mismatching version or name")
