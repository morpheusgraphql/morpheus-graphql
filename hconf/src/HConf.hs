{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf
  ( setup,
    SetupEnv (..),
  )
where

import Data.Text
import HConf.Env (SetupEnv (..))
import HConf.Hie (genHie)
import HConf.Package (checkPackages)
import HConf.Stack (setupStack)
import HConf.Version (parseVersion)
import HConf.Yaml (open, save)
import Prelude

setup :: String -> SetupEnv -> IO ()
setup ver = open $ do
  parseVersion (pack ver) >>= setupStack
  genHie
  checkPackages
  save
