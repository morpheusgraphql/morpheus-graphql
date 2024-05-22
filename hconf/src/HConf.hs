{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf
  ( setup,
    Env (..),
    updateVersion,
    Version,
    Parse (..),
    getVersion,
  )
where

import HConf.Config (Config (..), updateConfig)
import HConf.ConfigT (HCEnv (..))
import HConf.Env (Env (..))
import HConf.Hie (genHie)
import HConf.Log (log)
import HConf.Package (checkPackages)
import HConf.Stack (setupStack)
import HConf.Version (Parse (..), Version)
import HConf.Yaml (run, runSilent)
import Relude

setup :: String -> Env -> IO ()
setup v = run "setup" $ do
  parse v >>= setupStack
  genHie
  checkPackages
  pure Nothing

updateVersion :: String -> Bool -> Env -> IO ()
updateVersion v isBreaking = run "next" $ do
  v2 <- parse v
  newConfig <- asks config >>= updateConfig v2 isBreaking
  pure (Just newConfig)

getVersion :: Env -> IO ()
getVersion = runSilent $ do
  cfg <- asks config
  log (toString $ version cfg)
  pure Nothing
