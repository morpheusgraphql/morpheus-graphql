{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.Build
  ( Build (..),
    Builds,
    findBuild,
    getExtras,
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
import Data.Text (unpack)
import HConf.Config.Tag (VersionTag)
import HConf.Core.Version (Version, checkVersion)
import HConf.Utils.Class (Check (..))
import HConf.Utils.Core (notElemError)
import Relude hiding
  ( Undefined,
    group,
    intercalate,
    isPrefixOf,
  )

type Extras = Map Text Version

data Build = Build
  { ghc :: VersionTag,
    resolver :: Text,
    extra :: Maybe Extras,
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

instance Check Build where
  check Build {..} =
    traverse_
      (checkVersion . first unpack)
      (maybe [] M.toList extra)

type Builds = [Build]

findBuild :: (MonadFail m) => VersionTag -> Builds -> m Build
findBuild v builds = maybe (notElemError "build" (show v) (map ghc builds)) pure (find ((== v) . ghc) builds)

selectBuilds :: VersionTag -> [Build] -> [Build]
selectBuilds v = sortBy (\a b -> compare (ghc b) (ghc a)) . filter ((v <=) . ghc)

getExtras :: VersionTag -> [Build] -> Extras
getExtras version = M.fromList . concatMap getExtra . selectBuilds version

getExtra :: Build -> [(Text, Version)]
getExtra b = maybe [] M.toList (extra b)
