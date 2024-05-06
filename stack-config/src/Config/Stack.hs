{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Config.Stack
  ( Stack,
    updateStack,
  )
where

import Config.File
import Config.Types (Build (..), Config, getBuild, getBuilds, getPackages)
import Config.Version (Version (..), parseVersion)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.List ((\\))
import qualified Data.Map as M
import Relude hiding (Undefined, intercalate)

type Stack = Yaml StackType

data StackType = StackType
  { packages :: [Text],
    resolver :: Text,
    allowNewer :: Maybe Bool,
    saveHackageCreds :: Maybe Bool,
    extraDeps :: [Text]
  }
  deriving
    ( Show,
      Generic
    )

instance FromJSON StackType where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON StackType where
  toJSON = genericToJSON aesonYAMLOptions

updateStack :: (MonadFail m) => Text -> Config -> Stack -> m Stack
updateStack v config (Yaml _ yaml) = do
  version <- parseVersion v
  Build {..} <- getBuild version config
  extraDeps <- getExtraDeps version <$> getBuilds config
  let packages = (getPackages config <> maybeList include) \\ maybeList exclude
  pure
    $ Yaml
      StackType
        { packages,
          resolver,
          allowNewer = Just (getAllowNewer version),
          saveHackageCreds = Just False,
          extraDeps
        }
      yaml

getAllowNewer :: Version -> Bool
getAllowNewer LatestVersion = True
getAllowNewer _ = False

getExtraDeps :: Version -> [(Version, Build)] -> [Text]
getExtraDeps v xs = sort $ map printExtra $ concatMap f $ filter includeVersion xs
  where
    includeVersion (k, _) = v <= k
    f (_, b) = maybe [] M.toList (extra b)

printExtra :: (Text, Version) -> Text
printExtra (k, ver) = k <> "-" <> show ver