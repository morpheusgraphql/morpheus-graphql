{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
import HConf.Config.Build (Build (..))
import HConf.Config.Config (getBuild, getBuilds, getPackages)
import HConf.ConfigT (ConfigT, HCEnv (..))
import HConf.Core.Version (Version (..))
import HConf.Env (Env (..))
import HConf.Utils (Name, maybeList)
import HConf.Utils.Log (label, task)
import HConf.Yaml (aesonYAMLOptions, rewriteYaml)
import Relude

data Stack = Stack
  { packages :: [Name],
    resolver :: Name,
    allowNewer :: Maybe Bool,
    saveHackageCreds :: Maybe Bool,
    extraDeps :: [Name]
  }
  deriving
    ( Show,
      Generic
    )

instance FromJSON Stack where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON Stack where
  toJSON = genericToJSON aesonYAMLOptions

setupStack :: Version -> ConfigT ()
setupStack version = label ("stack(" <> show version <> ")") $ task "stack.yaml" $ do
  p <- asks (stack . env)
  rewriteYaml p (updateStack version)

updateStack :: Version -> Stack -> ConfigT Stack
updateStack version _ = do
  config <- asks config
  Build {..} <- getBuild version config
  extraDeps <- getExtraDeps version <$> getBuilds config
  let packages = (getPackages config <> maybeList include) \\ maybeList exclude
  pure
    Stack
      { packages,
        resolver,
        allowNewer = Just (LatestVersion == version),
        saveHackageCreds = Just False,
        extraDeps
      }

getExtraDeps :: Version -> [(Version, Build)] -> [Text]
getExtraDeps v = sort . concatMap (getExtra . snd) . filter (isHigher v)

isHigher :: Version -> (Version, b) -> Bool
isHigher v (k, _) = v <= k

getExtra :: Build -> [Text]
getExtra b = map printExtra $ maybe [] M.toList (extra b)

printExtra :: (Text, Version) -> Text
printExtra (k, ver) = k <> "-" <> show ver