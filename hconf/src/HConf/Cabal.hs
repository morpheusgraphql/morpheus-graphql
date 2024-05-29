{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Cabal
  ( checkCabals,
  )
where

import qualified Data.ByteString.Char8 as BS (unpack)
import Data.Map (lookup)
import Data.Text (breakOn, drop, isPrefixOf, null, pack, split, strip, unpack)
import GHC.IO.Exception (ExitCode (..))
import GHC.IO.Handle (hGetContents, hGetContents', hGetLine, hShow)
import HConf.ConfigT (ConfigT)
import HConf.Log (Log (log), alert, field, info, label, task)
import HConf.Package (Package (..), resolvePackages)
import HConf.Utils (Name)
import HConf.Version (Parse (..), Version)
import HConf.Yaml (read)
import Relude hiding (Undefined, break, drop, isPrefixOf, length, null, replicate)
import System.Process

parseFields :: ByteString -> Map Text Text
parseFields =
  fromList
    . filter (not . null . fst)
    . map (bimap strip strip . (second (drop 1) . breakOn ":") . strip)
    . split (== '\n')
    . pack
    . BS.unpack

getField :: (MonadFail m) => Name -> Map Name a -> m a
getField k = maybe (fail $ "missing field" <> unpack k) pure . lookup k

getCabalFields :: FilePath -> Name -> ConfigT (Name, Version)
getCabalFields path pkgName = do
  bs <- read (path <> "/" <> unpack pkgName <> ".cabal")
  let fields = parseFields bs
  name <- getField "name" fields
  version <- getField "version" fields >>= parse
  field (unpack name) (show version)
  pure (name, version)

noNewLine :: Char -> String
noNewLine '\n' = "          \n"
noNewLine x = [x]

stack :: String -> String -> [String] -> ConfigT ()
stack l name options = do
  (code, _, out) <- liftIO (readProcessWithExitCode "stack" (l : (name : map ("--" <>) options)) "")
  case code of
    ExitFailure {} -> alert (l <> ": " <> concatMap noNewLine (unpack $ strip $ pack out))
    ExitSuccess {} -> field l ("ok" <> show (parseOutput $ pack out))

parseOutput = dropWhile (not . isWarning) . split (== '\n')

isWarning x = isPrefixOf "Warning" x || isPrefixOf "warning" x

buildCabal :: String -> ConfigT ()
buildCabal name = do
  stack "build" name ["test", "dry-run"]
  stack "sdist" name []

checkCabal :: (Name, Package) -> ConfigT ()
checkCabal (path, Package {..}) = task path $ do
  buildCabal (unpack path)
  (pkgName, pkgVersion) <- getCabalFields (unpack path) name
  if pkgVersion == version && pkgName == name then pure () else fail (unpack path <> "mismatching version or name")

checkCabals :: ConfigT ()
checkCabals = label "cabal" $ resolvePackages >>= traverse_ checkCabal
