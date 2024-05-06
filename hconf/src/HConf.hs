{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf
  ( setup,
    SetupPath (..),
  )
where

import Data.Text
import HConf.Hie (genHie)
import HConf.Package (checkPackages)
import HConf.Stack (setupStack)
import HConf.Version (parseVersion)
import HConf.Yaml (readYaml, writeYaml)
import Prelude

data SetupPath = SetupPath
  { hie :: FilePath,
    hconf :: FilePath,
    stack :: FilePath
  }

setup :: SetupPath -> String -> IO ()
setup SetupPath {..} ver = do
  version <- parseVersion (pack ver)
  config <- readYaml hconf
  writeYaml hconf config
  setupStack stack version config
  genHie hie (pack stack) config
  checkPackages config