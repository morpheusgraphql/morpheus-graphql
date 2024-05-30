{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf
  ( setup,
    Env (..),
    updateVersion,
    Version,
    Parse (..),
    getVersion,
    upperBounds,
  )
where

import HConf.Cabal (checkCabals)
import HConf.Config.Config (Config (..), updateConfig, updateConfigUpperBounds)
import HConf.ConfigT (HCEnv (..))
import HConf.Core.Version (Version)
import HConf.Env (Env (..))
import HConf.Hie (genHie)
import HConf.Package (checkPackages)
import HConf.Stack (setupStack)
import HConf.Utils.Log (log)
import HConf.Utils.Parse (Parse (..))
import HConf.Yaml (run, runSilent)
import Relude

upperBounds :: Env -> IO ()
upperBounds = run "upper-bounds" $ do
  newConfig <- asks config >>= updateConfigUpperBounds
  pure (Just newConfig)

setup :: String -> Env -> IO ()
setup v = run "setup" $ do
  parse v >>= setupStack
  genHie
  checkPackages
  checkCabals
  pure Nothing

updateVersion :: Bool -> Env -> IO ()
updateVersion isBreaking = run "next" $ do
  newConfig <- asks config >>= updateConfig isBreaking
  pure (Just newConfig)

getVersion :: Env -> IO ()
getVersion = runSilent $ do
  cfg <- asks config
  log (toString $ version cfg)
  pure Nothing
