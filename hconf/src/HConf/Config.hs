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
import Data.List (maximum)
import qualified Data.Map as M
import Data.Text (isPrefixOf, unpack)
import HConf.Http (fetchVersions, getLatestBound)
import HConf.Log (Log (..), field)
import HConf.Utils (Name)
import HConf.Version
  ( Bound (Bound),
    Bounds (..),
    Deps,
    Restriction (..),
    Version,
    getBound,
    getDep,
    nextVersion,
    parse,
    traverseDeps,
  )
import Relude hiding
  ( Undefined,
    group,
    intercalate,
    isPrefixOf,
  )

data PkgGroup = PkgGroup
  { group :: Name,
    dir :: Text,
    packages :: [Text],
    prefix :: Maybe Bool
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

isLocalPackage :: Name -> Config -> Bool
isLocalPackage name Config {groups} = any ((`isPrefixOf` name) . group) groups

data Config = Config
  { version :: Version,
    bounds :: Bounds,
    groups :: [PkgGroup],
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
getPackages Config {..} = concatMap toPkg groups
  where
    toPkg PkgGroup {..} = map fullName packages
      where
        fullName s
          | dir /= "./" = dir <> "/" <> withPrefix s (fromMaybe False prefix) group
          | otherwise = withPrefix s (fromMaybe False prefix) group

getBuild :: (MonadFail m) => Version -> Config -> m Build
getBuild key Config {builds} = maybe (fail "invalid version") pure (M.lookup (show key) builds)

getBuilds :: (MonadFail m) => Config -> m [(Version, Build)]
getBuilds Config {builds} = traverse (\(k, v) -> (,v) <$> parse k) (M.toList builds)

withPrefix :: Text -> Bool -> Text -> Text
withPrefix "." True prefix = prefix
withPrefix s True prefix = prefix <> "-" <> s
withPrefix s _ _ = s

instance ToJSON Config where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

checkVersion :: (MonadFail m, MonadIO m) => (Text, Version) -> m ()
checkVersion (name, ver) =
  fetchVersions (unpack name)
    >>= \vs ->
      if ver `elem` vs
        then pure ()
        else fail ("no matching version for " <> unpack name)

checkBuild :: (MonadFail f, MonadIO f) => Build -> f [()]
checkBuild Build {..} = traverse checkVersion $ maybe [] M.toList extra

checkConfig :: (MonadFail f, MonadIO f) => Config -> f ()
checkConfig Config {..} = traverse_ checkBuild (toList builds)

updateConfig :: (MonadFail m, MonadIO m) => Bool -> Config -> m Config
updateConfig isBreaking Config {..} = do
  next <- nextVersion isBreaking version
  newBounds <- getBounds next
  pure Config {version = next, bounds = newBounds, ..}
  where
    getBounds next
      | isBreaking = do
          upper <- nextVersion True next
          pure $ Bounds [Bound Min True next, Bound Max False upper]
      | otherwise = pure bounds

updateConfigUpperBounds :: (MonadFail m, MonadIO m, Log m) => Config -> m Config
updateConfigUpperBounds Config {..} = do
  newDependencies <- traverseDeps upperBound dependencies
  pure Config {dependencies = newDependencies, ..}

upperBound :: (MonadFail m, MonadIO m, Log m) => Text -> Bounds -> m Bounds
upperBound name bounds = do
  latest <- getLatestBound name
  let ma = getBound Max bounds
  let mi = maybeToList (getBound Min bounds)
  let newVersion = maximum (latest : maybeToList ma)
  if ma == Just newVersion then pure () else field (unpack name) (show newVersion)
  pure (Bounds (mi <> [newVersion]))
