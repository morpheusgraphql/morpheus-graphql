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
import HConf.Yaml (readYaml, writeYaml)
import Prelude

setup :: SetupEnv -> String -> IO ()
setup env@SetupEnv {..} ver = do
  version <- parseVersion (pack ver)
  config <- readYaml hconf
  writeYaml hconf config
  setupStack stack version config
  genHie env config
  checkPackages config