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
import HConf.ConfigT
import HConf.Log
import HConf.Version (Version, VersionBounds (..))
import HConf.Yaml (Yaml (..), aesonYAMLOptions)
import Relude hiding
  ( Undefined,
    intercalate,
    isPrefixOf,
    length,
  )

type Table = [[Text]]

type Dependencies = [Text]

data LibType = LibType
  { sourceDirs :: Text,
    dependencies :: Maybe Dependencies
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

getSizes :: Table -> [Int]
getSizes xs = map size (transpose xs)
  where
    size :: [Text] -> Int
    size = maximum . map length

printRow :: [Int] -> Dependencies -> Text
printRow sizes ls =
  strip
    $ intercalate "  "
    $ map
      (\(item, s) -> justifyLeft s ' ' item)
      (zip ls sizes)

formatDependencies :: Table -> Dependencies
formatDependencies deps = map (printRow (getSizes deps)) deps

updateDependencies :: Dependencies -> ConfigT Dependencies
updateDependencies =
  fmap formatDependencies
    . traverse (withConfig checkDependency . filter (/= "") . split isSeparator)
    . sort

withRule1 :: Text -> VersionBounds -> ConfigT Dependencies
withRule1 name NoBounds = pure [name]
withRule1 name (VersionBounds mi ma) = pure $ printMin name mi <> printMax ma

withRule :: [Text] -> Text -> VersionBounds -> ConfigT Dependencies
withRule old name bounds = do
  deps <- withRule1 name bounds
  if old /= deps then field (toString name) "changed" else pure ()
  pure deps

printMin :: Text -> Version -> Dependencies
printMin name mi = [name, ">=", show mi]

printMax :: Maybe Version -> Dependencies
printMax = maybe [] (\m -> ["&&", "<", show m])

checkDependency :: Config -> Dependencies -> ConfigT Dependencies
checkDependency config@Config {name, bounds} (n : xs)
  | isPrefixOf name n && null xs = pure [n]
  | isPrefixOf name n = withRule xs n bounds
  | otherwise = getRule n config >>= withRule xs n
checkDependency _ [] = pure []

updateLib :: LibType -> ConfigT LibType
updateLib LibType {..} = do
  newDependencies <- traverse updateDependencies dependencies
  pure $ LibType {dependencies = newDependencies, ..}
