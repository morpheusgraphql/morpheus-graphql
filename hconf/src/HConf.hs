{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf
  ( setup,
    Env (..),
  )
where

import HConf.Env (Env (..))
import HConf.Hie (genHie)
import HConf.Package (checkPackages)
import HConf.Stack (setupStack)
import HConf.Version (Parse (..))
import HConf.Yaml (run)
import Prelude

setup :: String -> Env -> IO ()
setup version = run "setup" $ do
  parse version >>= setupStack
  genHie
  checkPackages
