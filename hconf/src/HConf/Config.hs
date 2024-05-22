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
import HConf.Utils
import HConf.Version
  ( Bounds (..),
    Deps,
    Version,
    getDep,
    nextVersion,
    parse,
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

updateConfig :: (MonadFail m) => Version -> Bool -> Config -> m Config
updateConfig prev isBreaking Config {..}
  | prev == version = do
      next <- getNext
      newBounds <- getBounds next
      pure Config {version = next, bounds = newBounds, ..}
  | otherwise = fail $ "versions does not match: " <> show version <> " != " <> show prev
  where
    getNext = nextVersion isBreaking prev
    getBounds next
      | isBreaking = Bounds next . Just <$> nextVersion True next
      | otherwise = pure bounds
