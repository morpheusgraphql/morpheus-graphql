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
import HConf.Env (Env (..))
import HConf.Hie (genHie)
import HConf.Log (log)
import HConf.Package (checkPackages)
import HConf.Parse (Parse (..))
import HConf.Stack (setupStack)
import HConf.Version (Version)
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
