{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.Build
  ( Build (..),
    Builds,
    checkBuild,
    findBuild,
    selectBuilds,
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
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Text (unpack)
import HConf.Core.Version (Version, fetchVersions)
import Relude hiding
  ( Undefined,
    group,
    intercalate,
    isPrefixOf,
  )

data Build = Build
  { ghc :: Version,
    resolver :: Text,
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

checkVersion :: (MonadFail m, MonadIO m) => (String, Version) -> m ()
checkVersion (name, version) =
  fetchVersions name
    >>= \vs ->
      if version `elem` vs
        then pure ()
        else
          fail
            ( "no matching version for "
                <> name
                <> "try one of:"
                <> intercalate ", " (map toString $ toList vs)
            )

checkBuild :: (MonadFail f, MonadIO f) => Build -> f ()
checkBuild Build {..} = traverse_ (checkVersion . first unpack) (maybe [] M.toList extra)

type Builds = [Build]

findBuild :: (MonadFail m) => Version -> Builds -> m Build
findBuild v builds = maybe (fail $ "no build found with version: " <> show v <> "!") pure (find ((== v) . ghc) builds)

selectBuilds :: Version -> [Build] -> [Build]
selectBuilds v = filter ((v <=) . ghc)
