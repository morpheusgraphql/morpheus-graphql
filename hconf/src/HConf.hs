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

import HConf.Cabal (checkCabals)
import HConf.Config (Config (..), updateConfig, updateConfigUpperBounds)
import HConf.ConfigT (HCEnv (..))
import HConf.Env (Env (..))
import HConf.Hie (genHie)
import HConf.Log (log)
import HConf.Package (checkPackages)
import HConf.Stack (setupStack)
import HConf.Version (Parse (..), Version)
import HConf.Yaml (run, runSilent)
import Relude

increaseUpperBounds :: Env -> IO ()
increaseUpperBounds = run "increase-upper-bounds" $ do
  newConfig <- asks config >>= updateConfigUpperBounds
  pure (Just newConfig)

setup :: String -> Env -> IO ()
setup v e = do
  increaseUpperBounds e
  run
    "setup"
    ( do
        parse v >>= setupStack
        genHie
        checkPackages
        checkCabals
        pure Nothing
    )
    e

updateVersion :: Bool -> Env -> IO ()
updateVersion isBreaking = run "next" $ do
  newConfig <- asks config >>= updateConfig isBreaking
  pure (Just newConfig)

getVersion :: Env -> IO ()
getVersion = runSilent $ do
  cfg <- asks config
  log (toString $ version cfg)
  pure Nothing
