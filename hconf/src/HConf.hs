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
import HConf.Log (label)
import HConf.Package (checkPackages)
import HConf.Stack (setupStack)
import HConf.Version (parseVersion)
import HConf.Yaml (open, save)
import Prelude

setup :: String -> Env -> IO ()
setup ver =
  open $ do
    label "setup" $ do
      parseVersion (pack ver) >>= setupStack
      genHie
      checkPackages
      save
