{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config.Lib
  ( LibType (..),
    updateDependencies,
    updateLib,
    Lib,
  )
where

import Config.File (Yaml (..), aesonYAMLOptions)
import Config.Types (Config (..), getBounds, getRule)
import Config.Version (VersionBounds (..))
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Char (isSeparator)
import Data.List (maximum)
import Data.Text (intercalate, isPrefixOf, length, replicate, split, strip)
import Relude hiding (Undefined, intercalate, isPrefixOf, length, replicate)

data LibType = LibType
  { sourceDirs :: Text,
    dependencies :: Maybe [Text]
  }
  deriving
    ( Show,
      Generic
    )

type Lib = Yaml LibType

instance FromJSON LibType where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON LibType where
  toJSON = genericToJSON aesonYAMLOptions

fill :: Int -> Text
fill n = replicate n " "

getSizes :: [[Text]] -> [Int]
getSizes xs = map size (transpose xs)
  where
    size :: [Text] -> Int
    size = maximum . map length

printRow :: [Int] -> [Text] -> Text
printRow sizes ls =
  strip
    $ intercalate "  "
    $ map
      (\(item, s) -> item <> fill (s - length item))
      (zip ls sizes)

formatDependencies :: [[Text]] -> [Text]
formatDependencies deps = map (printRow (getSizes deps)) deps

updateDependencies :: Config -> [Text] -> [Text]
updateDependencies config =
  formatDependencies
    . map (checkDependency config . filter (/= "") . split isSeparator)
    . sort

withRule :: Text -> VersionBounds -> [Text]
withRule name NoBounds = [name]
withRule name (VersionBounds mi (Just ma)) = [name, ">=", show mi, "&&", "<", show ma]
withRule name (VersionBounds mi Nothing) = [name, ">=", show mi]

checkDependency :: Config -> [Text] -> [Text]
checkDependency config@Config {name} (n : xs)
  | isPrefixOf name n && null xs = [n]
  | isPrefixOf name n = (getBounds config) >>= withRule n
  | otherwise = getRule n config >>= withRule n
checkDependency _ [] = []

updateLib :: Config -> Lib -> Lib
updateLib config (Yaml LibType {..} x) =
  ( Yaml
      LibType
        { dependencies =
            fmap (updateDependencies config) dependencies,
          ..
        }
      x
  )
