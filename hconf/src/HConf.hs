{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf
  ( setup,
    Env (..),
  )
where

import Data.Text
import HConf.Env (Env (..))
import HConf.Hie (genHie)
import HConf.Package (checkPackages)
import HConf.Stack (setupStack)
import HConf.Version (parseVersion)
import HConf.Yaml (run)
import Prelude

setup :: String -> Env -> IO ()
setup ver =
  run "setup" $ do
    parseVersion (pack ver) >>= setupStack
    genHie
    checkPackages
