{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Stack.Config
  ( Stack,
    setupStack,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.List ((\\))
import qualified Data.Map as M
import HConf.Config.Build (Build (..), selectBuilds)
import HConf.Config.Config (Config (builds), getBuild, getPackages)
import HConf.Config.ConfigT (ConfigT, HCEnv (..))
import HConf.Core.Env (Env (..))
import HConf.Core.Version (Version (..), VersionNumber)
import HConf.Utils.Core (Name, aesonYAMLOptions, maybeList)
import HConf.Utils.Log (label, task)
import HConf.Utils.Yaml (rewriteYaml)
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
  let packages = (getPackages config <> maybeList include) \\ maybeList exclude
  pure
    Stack
      { packages,
        resolver,
        allowNewer = Just (LatestVersion == version),
        saveHackageCreds = Just False,
        extraDeps = sort $ concatMap getExtra $ selectBuilds version $ builds config
      }

getExtra :: Build -> [Text]
getExtra b = map printExtra $ maybe [] M.toList (extra b)

printExtra :: (Text, VersionNumber) -> Text
printExtra (k, ver) = k <> "-" <> show ver