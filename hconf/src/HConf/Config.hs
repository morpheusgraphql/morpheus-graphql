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
    Build (..),
    getPackages,
    getBuild,
    getBuilds,
    getVersion,
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
import Data.Aeson.Types
  ( defaultOptions,
  )
import Data.List (maximum)
import qualified Data.Map as M
import Data.Text (unpack)
import HConf.Http (getLatestVersion)
import HConf.Utils
import HConf.Version
  ( Bounds (..),
    Deps,
    Version,
    getDep,
    nextVersion,
    parse,
    traverseDeps,
  )
import Relude hiding
  ( Undefined,
    intercalate,
  )

data PkgGroup = PkgGroup
  { dir :: Text,
    names :: [Text],
    prefix :: Maybe Text
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

instance ToJSON PkgGroup where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

data Build = Build
  { resolver :: Text,
    extra :: Maybe (Map Text Version),
    include :: Maybe [Text],
    exclude :: Maybe [Text]
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

instance ToJSON Build where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

data Config = Config
  { name :: Name,
    version :: Version,
    bounds :: Bounds,
    packages :: [PkgGroup],
    builds :: Map Text Build,
    dependencies :: Deps
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

getVersion :: Config -> Version
getVersion = version

getRule :: (MonadFail m) => Text -> Config -> m Bounds
getRule name = getDep name . dependencies

getPackages :: Config -> [Text]
getPackages Config {..} = concatMap toPkg packages
  where
    toPkg PkgGroup {..} = map fullName names
      where
        fullName s
          | dir /= "./" = dir <> "/" <> withPrefix s prefix
          | otherwise = withPrefix s prefix

getBuild :: (MonadFail m) => Version -> Config -> m Build
getBuild key Config {builds} = maybe (fail "invalid version") pure (M.lookup (show key) builds)

getBuilds :: (MonadFail m) => Config -> m [(Version, Build)]
getBuilds Config {builds} = traverse (\(k, v) -> (,v) <$> parse k) (M.toList builds)

withPrefix :: Text -> Maybe Text -> Text
withPrefix "." (Just prefix) = prefix
withPrefix s (Just prefix) = prefix <> "-" <> s
withPrefix s _ = s

instance ToJSON Config where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

updateConfig :: (MonadFail m) => Bool -> Config -> m Config
updateConfig isBreaking Config {..} = do
  next <- nextVersion isBreaking version
  newBounds <- getBounds next
  pure Config {version = next, bounds = newBounds, ..}
  where
    getBounds next
      | isBreaking = Bounds next . Just <$> nextVersion True next
      | otherwise = pure bounds

updateConfigUpperBounds :: (MonadFail m, MonadIO m) => Config -> m Config
updateConfigUpperBounds Config {..} = do
  newDependencies <- traverseDeps upperBound dependencies
  pure Config {dependencies = newDependencies, ..}

upperBound :: (MonadFail m, MonadIO m) => Text -> Bounds -> m Bounds
upperBound name (Bounds mi ma) = do
  latest <- getLatestVersion (unpack name)
  let versions = maximum (latest : maybeToList ma)
  pure (Bounds mi (Just versions))
upperBound _ b = pure b