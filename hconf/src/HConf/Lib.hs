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
import HConf.Log
import HConf.Utils (Name)
import HConf.Version
  ( Bounds (..),
    Deps,
    diff,
    traverseDeps,
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

data Lib = Lib
  { sourceDirs :: Text,
    dependencies :: Maybe Deps,
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
extendedToObject f t = Object (toObject (genericToJSON aesonYAMLOptions t) <> fromMaybe mempty (f t))

fromObject :: (Generic a, GFromJSON Zero (Rep a)) => (a -> Maybe Object -> a) -> Value -> Parser a
fromObject f v = do
  t <- genericParseJSON aesonYAMLOptions v
  o <- withObject "Lib" pure v
  pure (f t (Just o))

toObject :: Value -> Object
toObject (Object x) = delete "__unknown-fields" x
toObject _ = mempty

updateDependencies :: Deps -> ConfigT Deps
updateDependencies = traverseDeps (curry (withConfig checkDependency))

withRule :: Text -> Bounds -> Bounds -> ConfigT Bounds
withRule name old deps =
  when (old /= deps) (field (toString name) (diff old deps))
    $> deps

checkDependency :: Config -> (Name, Bounds) -> ConfigT Bounds
checkDependency config@Config {name, bounds} (depName, oldBounds)
  | name `isPrefixOf` depName = withRule depName oldBounds bounds
  | otherwise = getRule depName config >>= withRule depName oldBounds

updateLib :: Lib -> ConfigT Lib
updateLib Lib {..} = do
  newDependencies <- traverse updateDependencies dependencies
  pure $ Lib {dependencies = newDependencies, ..}
