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

data Dependency
  = Dependency {minV :: Text, maxV :: Maybe Text}
  deriving (Show)

type Dep = Maybe (Text, Maybe Dependency)

parseDep :: Text -> Dep
parseDep txt =
  let xs = filter (/= "") (split isSeparator txt)
   in decode xs
  where
    decode :: [Text] -> Dep
    decode [] = Nothing
    decode (name : bounds) = Just (name, parseBounds bounds)

    parseBounds :: [Text] -> Maybe Dependency
    parseBounds (">" : mi : ls) = Just (Dependency mi (parseMax ls))
    parseBounds (">=" : mi : ls) = Just (Dependency mi (parseMax ls))
    parseBounds _ = Nothing
    parseMax :: [Text] -> Maybe Text
    parseMax ("&&" : "<=" : x : _) = Just x
    parseMax ("&&" : "<" : x : _) = Just x
    parseMax _ = Nothing

updateDependencies :: TextDeps -> ConfigT TextDeps
updateDependencies = fmap formatDependencies . traverse check . sort

check :: Text -> ConfigT TextDeps
check x = do
  log (show $ parseDep x)
  (withConfig checkDependency . filter (/= "") . split isSeparator) x

genBounds :: VersionBounds -> ConfigT TextDeps
genBounds NoBounds = pure []
genBounds (VersionBounds mi ma) = pure $ printMin mi <> printMax ma

withRule :: [Text] -> Text -> VersionBounds -> ConfigT TextDeps
withRule old name bounds = do
  deps <- genBounds bounds
  if old /= deps then field (toString name) (logDep old <> "  ->  " <> logDep deps) else pure ()
  pure (name : deps)

logDep :: TextDeps -> String
logDep = toString . intercalate "  "

printMin :: Version -> TextDeps
printMin mi = [">=", show mi]

printMax :: Maybe Version -> TextDeps
printMax = maybe [] (\m -> ["&&", "<", show m])

checkDependency :: Config -> TextDeps -> ConfigT TextDeps
checkDependency config@Config {name, bounds} (n : xs)
  | isPrefixOf name n && null xs = pure [n]
  | isPrefixOf name n = withRule xs n bounds
  | otherwise = getRule n config >>= withRule xs n
checkDependency _ [] = pure []

updateLib :: LibType -> ConfigT LibType
updateLib LibType {..} = do
  newDependencies <- traverse updateDependencies dependencies
  pure $ LibType {dependencies = newDependencies, ..}
