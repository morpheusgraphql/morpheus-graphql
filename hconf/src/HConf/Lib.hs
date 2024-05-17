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
import HConf.Version (VersionBounds (..), parseBoundsFrom)
import HConf.Yaml (Yaml (..), aesonYAMLOptions)
import Relude hiding
  ( Undefined,
    intercalate,
    isPrefixOf,
    length,
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

type DepType = Maybe (Text, VersionBounds)

parseDep :: Text -> ConfigT DepType
parseDep txt =
  let xs = filter (/= "") (split isSeparator txt)
   in decode xs
  where
    decode :: [Text] -> ConfigT DepType
    decode [] = pure Nothing
    decode (name : bounds) = Just . (name,) <$> parseBounds bounds

    parseBounds :: [Text] -> ConfigT VersionBounds
    parseBounds (o : mi : ls) | isOperator o = parseMax ls >>= parseBoundsFrom mi
    parseBounds _ = pure NoBounds
    parseMax :: [Text] -> ConfigT (Maybe Text)
    parseMax (b : o : x : _) | b == "&&" && isUpperO o = pure (Just x)
    parseMax _ = pure Nothing

isOperator :: Text -> Bool
isOperator = (`elem` [">", ">="])

isUpperO :: Text -> Bool
isUpperO = (`elem` ["<", "<="])

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

checkDependency :: Config -> DepType -> ConfigT TextDeps
checkDependency config@Config {name, bounds} (Just (n, dp))
  | isPrefixOf name n && dp == NoBounds = pure [n]
  | isPrefixOf name n = withRule dp n bounds
  | otherwise = getRule n config >>= withRule dp n
checkDependency _ Nothing = pure []

updateLib :: LibType -> ConfigT LibType
updateLib LibType {..} = do
  newDependencies <- traverse updateDependencies dependencies
  pure $ LibType {dependencies = newDependencies, ..}
