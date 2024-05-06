{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Config.Types (Config)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Char (isSeparator)
import Data.List (maximum)
import Data.Text (intercalate, length, replicate, split, strip)
import Relude hiding (Undefined, intercalate, length, replicate)

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
updateDependencies _ = formatDependencies . map (checkDependency . filter (/= "") . split isSeparator) . sort

checkDependency :: [Text] -> [Text]
checkDependency = id

-- checkDependency(name: string, hasNoBounds: boolean): string[] {
--     if (name.startsWith(this.config.name)) {
--       if (hasNoBounds) {
--         return [name];
--       }
--       return withRule(name, this.config.bounds);
--     }

--     const rule = this.rule(name);

--     if (rule) {
--       return typeof rule === "boolean" ? [name] : withRule(name, rule);
--     }

--     throw new Error(`Unknown package: ${name}`);
--   }

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
