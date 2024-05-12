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
import HConf.Yaml (saveConfig, withConfig)
import Prelude

setup :: String -> SetupEnv -> IO ()
setup ver = withConfig $ do
  parseVersion (pack ver) >>= setupStack
  genHie
  checkPackages
  saveConfig
