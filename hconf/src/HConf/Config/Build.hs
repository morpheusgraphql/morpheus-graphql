{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.Build
  ( Build (..),
    Builds,
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
import HConf.Config.VersionTag (VersionTag)
import HConf.Core.Version (VersionNumber, fetchVersions)
import HConf.Utils.Class (Check (..))
import Relude hiding
  ( Undefined,
    group,
    intercalate,
    isPrefixOf,
  )

data Build = Build
  { ghc :: VersionTag,
    resolver :: Text,
    extra :: Maybe (Map Text VersionNumber),
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

checkVersion :: (MonadFail m, MonadIO m) => (String, VersionNumber) -> m ()
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

instance Check Build where
  check Build {..} =
    traverse_ (checkVersion . first unpack) (maybe [] M.toList extra)

type Builds = [Build]

findBuild :: (MonadFail m) => VersionTag -> Builds -> m Build
findBuild v builds = maybe (fail $ "no build found with version: " <> show v <> "!") pure (find ((== v) . ghc) builds)

selectBuilds :: VersionTag -> [Build] -> [Build]
selectBuilds v = filter ((v <=) . ghc)
