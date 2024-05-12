{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf
  ( setup,
    SetupEnv (..),
  )
where

import Control.Monad.Reader.Class
import Data.Text
import HConf.Env (SetupEnv (..))
import HConf.Hie (genHie)
import HConf.Package (checkPackages)
import HConf.Stack (setupStack)
import HConf.Version (parseVersion)
import HConf.Yaml (withConfig, writeYaml)
import Prelude

setup :: SetupEnv -> String -> IO ()
setup env@SetupEnv {..} ver = withConfig env $
  do
    version <- parseVersion (pack ver)
    asks id >>= writeYaml hconf
    setupStack stack version
    genHie env
    checkPackages