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
import HConf.Config.Build (Build (..), getExtras)
import HConf.Config.Config (Config (builds), getBuild, getPackages)
import HConf.Config.ConfigT (ConfigT, HCEnv (..))
import HConf.Config.Tag (VersionTag (..))
import HConf.Core.Env (Env (..))
import HConf.Core.Version (Version)
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

setupStack :: VersionTag -> ConfigT ()
setupStack version = label ("stack(" <> show version <> ")") $ task "stack.yaml" $ do
  p <- asks (stack . env)
  rewriteYaml p (updateStack version) $> ()

updateStack :: VersionTag -> Stack -> ConfigT Stack
updateStack version _ = do
  config <- asks config
  Build {..} <- getBuild version config
  -- TODO: check if exclude /include packages exist
  let packages = (getPackages config <> maybeList include) \\ maybeList exclude
  pure
    Stack
      { packages,
        resolver,
        allowNewer = Just (Latest == version),
        saveHackageCreds = Just False,
        extraDeps = sort $ map printExtra $ M.toList $ getExtras version $ builds config
      }

printExtra :: (Text, Version) -> Text
printExtra (k, ver) = k <> "-" <> show ver
