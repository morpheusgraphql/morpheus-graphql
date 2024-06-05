{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Config.Config
  ( Config (..),
    getPackages,
    getBuild,
    getRule,
    updateConfig,
    updateConfigUpperBounds,
  )
where

import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (toJSON),
    genericToJSON,
  )
import Data.Aeson.Types (defaultOptions)
import HConf.Config.Build (Build, Builds, findBuild)
import HConf.Config.PkgGroup (PkgGroup, isMember, toPackageName)
import HConf.Config.Tag (VersionTag)
import HConf.Core.Bounds (Bounds, updateUpperBound, versionBounds)
import HConf.Core.Dependencies (Dependencies, getBounds, traverseDeps)
import HConf.Core.Version (Version, nextVersion)
import HConf.Utils.Class (Check (check))
import HConf.Utils.Log (Log (..))
import Relude hiding
  ( Undefined,
    group,
    intercalate,
    isPrefixOf,
  )

data Config = Config
  { version :: Version,
    bounds :: Bounds,
    groups :: [PkgGroup],
    builds :: Builds,
    dependencies :: Dependencies
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

getRule :: (MonadFail m) => Text -> Config -> m Bounds
getRule name Config {..}
  | any (isMember name) groups = pure bounds
  | otherwise = getBounds name dependencies

getPackages :: Config -> [Text]
getPackages Config {..} = concatMap toPackageName groups

getBuild :: (MonadFail m) => VersionTag -> Config -> m Build
getBuild key = findBuild key . builds

instance ToJSON Config where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

instance Check Config where
  check Config {..} = traverse_ check (toList builds)

updateConfig :: Bool -> Config -> Config
updateConfig isBreaking Config {..} =
  let version' = nextVersion isBreaking version
      bounds' = versionBounds version'
   in Config {version = version', bounds = bounds', ..}

updateConfigUpperBounds :: (MonadFail m, MonadIO m, Log m) => Config -> m Config
updateConfigUpperBounds Config {..} = do
  dependencies' <- traverseDeps updateUpperBound dependencies
  pure Config {dependencies = dependencies', ..}
