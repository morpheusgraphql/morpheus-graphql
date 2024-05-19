{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
import Data.Text
  ( isPrefixOf,
  )
import HConf.Config (Config (..), getRule)
import HConf.ConfigT
import HConf.Format (formatTable)
import HConf.Log
import HConf.Utils (Name)
import HConf.Version
  ( VersionBounds (..),
    parseDep,
    printBoundParts,
    printBounds,
  )
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

updateDependencies :: TextDeps -> ConfigT TextDeps
updateDependencies = fmap formatTable . traverse (parseDep >=> withConfig checkDependency)

withRule :: VersionBounds -> Text -> VersionBounds -> ConfigT TextDeps
withRule old name deps = do
  when (old /= deps) $ field (toString name) (printBounds old <> "  ->  " <> printBounds deps)
  pure (name : printBoundParts deps)

checkDependency :: Config -> (Name, VersionBounds) -> ConfigT TextDeps
checkDependency config@Config {name, bounds} (n, dp)
  | name `isPrefixOf` n && dp == NoBounds = pure [n]
  | name `isPrefixOf` n = withRule dp n bounds
  | otherwise = getRule n config >>= withRule dp n

updateLib :: LibType -> ConfigT LibType
updateLib LibType {..} = do
  newDependencies <- traverse updateDependencies dependencies
  pure $ LibType {dependencies = newDependencies, ..}
