{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Yaml
  ( run,
    runSilent,
  )
where

import HConf.Config.Config (Config)
import HConf.Config.ConfigT (ConfigT (..), HCEnv (..), runConfigT)
import HConf.Core.Env (Env (..))
import HConf.Utils.Class (Check (check))
import HConf.Utils.Log (alert, info, label, task)
import HConf.Utils.Yaml (readYaml, writeYaml)
import Relude hiding (Show, Undefined, intercalate, show)

runSilent :: ConfigT (Maybe Config) -> Env -> IO ()
runSilent t env@Env {..} = do
  cfg <- readYaml hconf
  res <- runConfigT t env cfg
  case res of
    Left x -> alert ("ERROR: " <> x)
    Right _ -> pure ()

run :: String -> ConfigT (Maybe Config) -> Env -> IO ()
run name t env@Env {..} = do
  cfg <- readYaml hconf
  check cfg
  res <- runConfigT (label name (t >>= save)) env cfg
  case res of
    Left x -> alert ("ERROR: " <> x)
    Right _ -> info "OK"

save :: Maybe Config -> ConfigT ()
save Nothing = pure ()
save (Just cfg) = label "hconf" $ task "hconf.yaml" $ do
  ctx <- asks id
  writeYaml (hconf $ env ctx) cfg
