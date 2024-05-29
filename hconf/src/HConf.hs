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
import HConf.Config (Config (..), updateConfig)
import HConf.ConfigT (HCEnv (..))
import HConf.Env (Env (..))
import HConf.Hie (genHie)
import HConf.Http (getLatestVersion)
import HConf.Log (log)
import HConf.Package (checkPackages)
import HConf.Stack (setupStack)
import HConf.Version (Parse (..), Version)
import HConf.Yaml (run, runSilent)
import Relude

setup :: String -> Env -> IO ()
setup v = run "setup" $ do
  x <- getLatestVersion "morpheus-graphql"
  log (show x)
  parse v >>= setupStack
  genHie
  checkPackages
  checkCabals
  newConfig <- asks config
  pure (Just newConfig)

updateVersion :: Bool -> Env -> IO ()
updateVersion isBreaking = run "next" $ do
  newConfig <- asks config >>= updateConfig isBreaking
  pure (Just newConfig)

getVersion :: Env -> IO ()
getVersion = runSilent $ do
  cfg <- asks config
  log (toString $ version cfg)
  pure Nothing
