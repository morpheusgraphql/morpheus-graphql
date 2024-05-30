{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Config
  ( Config (..),
    getPackages,
    getBuild,
    getBuilds,
    getVersion,
    getRule,
    updateConfig,
    updateConfigUpperBounds,
    isLocalPackage,
    checkConfig,
  )
where

import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (toJSON),
    genericToJSON,
  )
import Data.Aeson.Types
  ( defaultOptions,
  )
import qualified Data.Map as M
import HConf.Bounds (Bounds (..), updateUpperBound, versionBounds)
import HConf.Config.Build (Build, checkBuild)
import HConf.Config.PkgGroup (PkgGroup, isMember, toPackageName)
import HConf.Deps (Dependencies, getBounds, traverseDeps)
import HConf.Log (Log (..))
import HConf.Parse (Parse (..))
import HConf.Utils (Name)
import HConf.Version
  ( Version,
    nextVersion,
  )
import Relude hiding
  ( Undefined,
    group,
    intercalate,
    isPrefixOf,
  )

isLocalPackage :: Name -> Config -> Bool
isLocalPackage name Config {groups} = any (isMember name) groups

data Config = Config
  { version :: Version,
    bounds :: Bounds,
    groups :: [PkgGroup],
    builds :: Map Text Build,
    dependencies :: Dependencies
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

getVersion :: Config -> Version
getVersion = version

getRule :: (MonadFail m) => Text -> Config -> m Bounds
getRule name = getBounds name . dependencies

getPackages :: Config -> [Text]
getPackages Config {..} = concatMap toPackageName groups

getBuild :: (MonadFail m) => Version -> Config -> m Build
getBuild key Config {builds} = maybe (fail "invalid version") pure (M.lookup (show key) builds)

getBuilds :: (MonadFail m) => Config -> m [(Version, Build)]
getBuilds Config {builds} = traverse (\(k, v) -> (,v) <$> parseText k) (M.toList builds)

instance ToJSON Config where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

checkConfig :: (MonadFail f, MonadIO f) => Config -> f ()
checkConfig Config {..} = traverse_ checkBuild (toList builds)

updateConfig :: (MonadFail m, MonadIO m) => Bool -> Config -> m Config
updateConfig isBreaking Config {..} = do
  version' <- nextVersion isBreaking version
  bounds' <- versionBounds version'
  pure Config {version = version', bounds = bounds', ..}

updateConfigUpperBounds :: (MonadFail m, MonadIO m, Log m) => Config -> m Config
updateConfigUpperBounds Config {..} = do
  dependencies' <- traverseDeps updateUpperBound dependencies
  pure Config {dependencies = dependencies', ..}
