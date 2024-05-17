{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
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
import Data.List (maximum)
import Data.Text
  ( intercalate,
    isPrefixOf,
    justifyLeft,
    length,
    strip,
  )
import HConf.Config (Config (..), getRule)
import HConf.ConfigT
import HConf.Log
import HConf.Utils (Name)
import HConf.Version (VersionBounds (..), parseDep)
import HConf.Yaml (Yaml (..), aesonYAMLOptions)
import Relude hiding
  ( Undefined,
    break,
    drop,
    intercalate,
    isPrefixOf,
    length,
    null,
  )

type Table = [[Text]]

type TextDeps = [Text]

data LibType = LibType
  { sourceDirs :: Text,
    dependencies :: Maybe TextDeps
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

printRow :: [Int] -> TextDeps -> Text
printRow sizes ls =
  strip
    $ intercalate "  "
    $ map
      (\(item, s) -> justifyLeft s ' ' item)
      (zip ls sizes)

formatDependencies :: Table -> TextDeps
formatDependencies deps = map (printRow (getSizes deps)) deps

updateDependencies :: TextDeps -> ConfigT TextDeps
updateDependencies = fmap formatDependencies . traverse (parseDep >=> withConfig checkDependency) . sort

printDep :: VersionBounds -> TextDeps
printDep NoBounds = []
printDep (VersionBounds mi ma) = [">=", show mi] <> maybe [] (\m -> ["&&", "<", show m]) ma

withRule :: VersionBounds -> Text -> VersionBounds -> ConfigT TextDeps
withRule old name bounds = do
  let deps = bounds
  if old /= deps then field (toString name) (logDep old <> "  ->  " <> logDep deps) else pure ()
  pure (name : printDep deps)

logDep :: VersionBounds -> String
logDep = toString . intercalate "  " . printDep

checkDependency :: Config -> (Name, VersionBounds) -> ConfigT TextDeps
checkDependency config@Config {name, bounds} (n, dp)
  | isPrefixOf name n && dp == NoBounds = pure [n]
  | isPrefixOf name n = withRule dp n bounds
  | otherwise = getRule n config >>= withRule dp n

updateLib :: LibType -> ConfigT LibType
updateLib LibType {..} = do
  newDependencies <- traverse updateDependencies dependencies
  pure $ LibType {dependencies = newDependencies, ..}
