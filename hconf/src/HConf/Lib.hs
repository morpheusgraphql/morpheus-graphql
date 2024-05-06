{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Lib
  ( LibType (..),
    updateDependencies,
    updateLib,
    Lib,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    genericParseJSON,
    genericToJSON,
  )
import Data.Char (isSeparator)
import Data.List (maximum)
import Data.Text
  ( intercalate,
    isPrefixOf,
    justifyLeft,
    length,
    split,
    strip,
  )
import HConf.Config (Config (..), getRule)
import HConf.Version (Version, VersionBounds (..))
import HConf.Yaml (Yaml (..), aesonYAMLOptions)
import Relude hiding (Undefined, intercalate, isPrefixOf, length)

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
      (\(item, s) -> justifyLeft s ' ' item)
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
withRule name (VersionBounds mi ma) = printMin name mi <> printMax ma

printMin :: Text -> Version -> [Text]
printMin name mi = [name, ">=", show mi]

printMax :: Maybe Version -> [Text]
printMax = maybe [] (\m -> ["&&", "<", show m])

checkDependency :: Config -> [Text] -> [Text]
checkDependency config@Config {name, bounds} (n : xs)
  | isPrefixOf name n && null xs = [n]
  | isPrefixOf name n = withRule n bounds
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
