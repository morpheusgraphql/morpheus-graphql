{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Stack
  ( Stack,
    setupStack,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.List ((\\))
import qualified Data.Map as M
import HConf.Config (Build (..), Config, getBuild, getBuilds, getPackages)
import HConf.Utils (maybeList)
import HConf.Version (Version (..), parseVersion)
import HConf.Yaml (aesonYAMLOptions, rewriteYaml)
import Relude

data Stack = Stack
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

instance FromJSON Stack where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON Stack where
  toJSON = genericToJSON aesonYAMLOptions

setupStack :: FilePath -> Text -> Config -> IO ()
setupStack path version config = rewriteYaml path (updateStack version config)

updateStack :: (MonadFail m) => Text -> Config -> Stack -> m Stack
updateStack v config _ = do
  version <- parseVersion v
  Build {..} <- getBuild version config
  extraDeps <- getExtraDeps version <$> getBuilds config
  let packages = (getPackages config <> maybeList include) \\ maybeList exclude
  pure
    Stack
      { packages,
        resolver,
        allowNewer = Just (getAllowNewer version),
        saveHackageCreds = Just False,
        extraDeps
      }

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