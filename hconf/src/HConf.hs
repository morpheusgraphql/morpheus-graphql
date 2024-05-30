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
import HConf.Config.ConfigT (HCEnv (..), run, runSilent)
import HConf.Core.Env (Env (..))
import HConf.Core.Version (Version)
import HConf.Hie (genHie)
import HConf.Stack.Config (setupStack)
import HConf.Stack.Package (checkPackages)
import HConf.Utils.Class (Parse (..))
import HConf.Utils.Log (log)
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
