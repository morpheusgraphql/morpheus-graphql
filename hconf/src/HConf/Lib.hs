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
import qualified Data.Map as M
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
  ( Deps,
    VersionBounds (..),
    parseDep,
    printBoundParts,
    printBounds,
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

type TextDeps = [Text]

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
extendedToObject f t = Object (fromMaybe mempty (f t) <> toObject (genericToJSON aesonYAMLOptions t))

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

checkIfEq :: (Applicative f, Log f, ToString a) => a -> VersionBounds -> VersionBounds -> f ()
checkIfEq name old deps = when (old /= deps) $ field (toString name) (printBounds old <> "  ->  " <> printBounds deps)

withRule :: Text -> VersionBounds -> VersionBounds -> ConfigT VersionBounds
withRule name old deps = checkIfEq name old deps $> deps

checkDependency :: Config -> (Name, VersionBounds) -> ConfigT VersionBounds
checkDependency config@Config {name, bounds} (n, dp)
  | name `isPrefixOf` n && dp == NoBounds = pure NoBounds
  | name `isPrefixOf` n = withRule n dp bounds
  | otherwise = getRule n config >>= withRule n dp

updateLib :: Lib -> ConfigT Lib
updateLib Lib {..} = do
  newDependencies <- traverse updateDependencies dependencies
  pure $ Lib {dependencies = newDependencies, ..}
