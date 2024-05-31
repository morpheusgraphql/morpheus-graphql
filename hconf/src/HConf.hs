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
import HConf.Config.ConfigT (HCEnv (..), run, runTask, save)
import HConf.Core.Env (Env (..))
import HConf.Core.Version (Version)
import HConf.Hie (genHie)
import HConf.Stack.Config (setupStack)
import HConf.Stack.Package (checkPackages)
import HConf.Utils.Class (Parse (..))
import Relude

upperBounds :: Env -> IO ()
upperBounds =
  runTask "upper-bounds"
    $ asks config
    >>= updateConfigUpperBounds
    >>= save

setup :: String -> Env -> IO ()
setup v = runTask "setup" $ do
  parse v >>= setupStack
  genHie
  checkPackages
  checkCabals

updateVersion :: Bool -> Env -> IO ()
updateVersion isBreaking =
  runTask "next"
    $ asks config
    >>= updateConfig isBreaking
    >>= save

getVersion :: Env -> IO ()
getVersion = run (Just . version <$> asks config)
