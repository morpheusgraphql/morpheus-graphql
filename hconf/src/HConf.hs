{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf
  ( setup,
    Env (..),
    updateVersion,
  )
where

import HConf.Config (updateConfig)
import HConf.ConfigT (HCEnv (..))
import HConf.Env (Env (..))
import HConf.Hie (genHie)
import HConf.Log (info)
import HConf.Package (checkPackages)
import HConf.Stack (setupStack)
import HConf.Version (Parse (..))
import HConf.Yaml (run)
import Relude (asks)
import Prelude

setup :: String -> Env -> IO ()
setup version = run "setup" $ do
  parse version >>= setupStack
  genHie
  checkPackages
  pure Nothing

updateVersion :: String -> Bool -> Env -> IO ()
updateVersion v isBreaking = run "next" $ do
  version <- parse v
  info (show version)
  newConfig <- asks config >>= updateConfig version isBreaking
  pure (Just newConfig)
