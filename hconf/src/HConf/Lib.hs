{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Lib
  ( Lib (..),
    updateDependencies,
    updateLib,
  )
where

import Data.Aeson.KeyMap (delete)
import Data.Aeson.Types
import Data.Text
  ( isPrefixOf,
  )
import GHC.Generics
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
import HConf.Yaml (aesonYAMLOptions)
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

data Lib = Lib
  { sourceDirs :: Text,
    dependencies :: Maybe TextDeps,
    __unknownFields :: Maybe Object
  }
  deriving
    ( Show,
      Generic
    )

instance FromJSON Lib where
  parseJSON = fromObject (\t o -> t {__unknownFields = o})

instance ToJSON Lib where
  toJSON = extendedToObject __unknownFields

extendedToObject :: (Generic a, GToJSON' Value Zero (Rep a)) => (a -> Maybe Object) -> a -> Value
extendedToObject f t = Object (fromMaybe mempty (f t) <> toObject (genericToJSON aesonYAMLOptions t))

fromObject :: (Generic a, GFromJSON Zero (Rep a)) => (a -> Maybe Object -> a) -> Value -> Parser a
fromObject f v = do
  t <- genericParseJSON aesonYAMLOptions v
  o <- withObject "Lib" pure v
  pure (f t (Just o))

toObject :: Value -> Object
toObject (Object x) = delete "__unknown-fields" x
toObject _ = mempty

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

updateLib :: Lib -> ConfigT Lib
updateLib Lib {..} = do
  newDependencies <- traverse updateDependencies dependencies
  pure $ Lib {dependencies = newDependencies, ..}
