{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Stack.Lib
  ( Library (..),
    updateDependencies,
    updateLib,
  )
where

import Data.Aeson.KeyMap (delete)
import Data.Aeson.Types
import GHC.Generics
import HConf.Config.Config (getRule, localPkgBounds)
import HConf.Config.ConfigT
import HConf.Core.Bounds (Bounds (..), diff)
import HConf.Core.Dependencies (Dependencies, traverseDeps)
import HConf.Utils.Core (Name, aesonYAMLOptions)
import HConf.Utils.Log
import Relude hiding
  ( Undefined,
    break,
    drop,
    intercalate,
    isPrefixOf,
    length,
    null,
  )

data Library = Library
  { sourceDirs :: Text,
    dependencies :: Maybe Dependencies,
    __unknownFields :: Maybe Object
  }
  deriving
    ( Show,
      Generic
    )

instance FromJSON Library where
  parseJSON = fromObject (\t o -> t {__unknownFields = o})

instance ToJSON Library where
  toJSON = extendedToObject __unknownFields

extendedToObject :: (Generic a, GToJSON' Value Zero (Rep a)) => (a -> Maybe Object) -> a -> Value
extendedToObject f t = Object (toObject (genericToJSON aesonYAMLOptions t) <> fromMaybe mempty (f t))

fromObject :: (Generic a, GFromJSON Zero (Rep a)) => (a -> Maybe Object -> a) -> Value -> Parser a
fromObject f v = do
  t <- genericParseJSON aesonYAMLOptions v
  o <- withObject "Lib" pure v
  pure (f t (Just o))

toObject :: Value -> Object
toObject (Object x) = delete "__unknown-fields" x
toObject _ = mempty

withRule :: Text -> Bounds -> Bounds -> ConfigT Bounds
withRule name old deps =
  when (old /= deps) (field (toString name) (diff old deps))
    $> deps

updateDependency :: Name -> Bounds -> ConfigT Bounds
updateDependency name oldBounds = do
  cgf <- asks config
  bounds <- maybe (getRule name cgf) pure (localPkgBounds name cgf)
  withRule name oldBounds bounds

updateDependencies :: Dependencies -> ConfigT Dependencies
updateDependencies = traverseDeps updateDependency

updateLib :: Library -> ConfigT Library
updateLib Library {..} = do
  newDependencies <- traverse updateDependencies dependencies
  pure $ Library {dependencies = newDependencies, ..}
