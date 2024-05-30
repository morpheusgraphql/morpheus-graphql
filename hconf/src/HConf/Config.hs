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
import Data.List (intercalate, maximum)
import qualified Data.Map as M
import Data.Text (isPrefixOf, pack, unpack)
import HConf.Bounds (Bounds (..), Restriction (..), getBound, upperBounds)
import HConf.Deps (Dependencies, getBounds, traverseDeps)
import HConf.Http (fetchVersions, getLatestBound)
import HConf.Log (Log (..), field)
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
import System.FilePath.Posix (joinPath, normalise)

data PkgGroup = PkgGroup
  { group :: Name,
    dir :: Maybe FilePath,
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
isLocalPackage name Config {groups} =
  any ((`isPrefixOf` name) . group) groups

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
getPackages Config {..} = concatMap toPkg groups
  where
    toPkg PkgGroup {..} = map (pack . pkgPath) packages
      where
        pkgPath name =
          let pkgName = intercalate "-" ([unpack group | fromMaybe False prefix] <> [unpack name | name /= "."])
           in normalise (joinPath (maybeToList dir <> [pkgName]))

getBuild :: (MonadFail m) => Version -> Config -> m Build
getBuild key Config {builds} = maybe (fail "invalid version") pure (M.lookup (show key) builds)

getBuilds :: (MonadFail m) => Config -> m [(Version, Build)]
getBuilds Config {builds} = traverse (\(k, v) -> (,v) <$> parseText k) (M.toList builds)

instance ToJSON Config where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

checkVersion :: (MonadFail m, MonadIO m) => (Text, Version) -> m ()
checkVersion (name, ver) =
  fetchVersions (unpack name)
    >>= \vs ->
      if ver `elem` vs
        then pure ()
        else
          fail
            ( "no matching version for "
                <> unpack name
                <> "try one of:"
                <> intercalate ", " (map toString $ toList vs)
            )

checkBuild :: (MonadFail f, MonadIO f) => Build -> f [()]
checkBuild Build {..} = traverse checkVersion $ maybe [] M.toList extra

checkConfig :: (MonadFail f, MonadIO f) => Config -> f ()
checkConfig Config {..} = traverse_ checkBuild (toList builds)

updateConfig :: (MonadFail m, MonadIO m) => Bool -> Config -> m Config
updateConfig isBreaking Config {..} = do
  version' <- nextVersion isBreaking version
  bounds' <- updateBounds version' isBreaking bounds
  pure Config {version = version', bounds = bounds', ..}

updateBounds :: (MonadFail m) => Version -> Bool -> Bounds -> m Bounds
updateBounds v isBreaking bounds
  | isBreaking = upperBounds v
  | otherwise = pure bounds

updateConfigUpperBounds :: (MonadFail m, MonadIO m, Log m) => Config -> m Config
updateConfigUpperBounds Config {..} = do
  dependencies' <- traverseDeps upperBound dependencies
  pure Config {dependencies = dependencies', ..}

upperBound :: (MonadFail m, MonadIO m, Log m) => Text -> Bounds -> m Bounds
upperBound name bounds = do
  latest <- getLatestBound name
  let ma = getBound Max bounds
  let mi = maybeToList (getBound Min bounds)
  let newVersion = maximum (latest : maybeToList ma)
  if ma == Just newVersion then pure () else field (unpack name) (show newVersion)
  pure (Bounds (mi <> [newVersion]))
