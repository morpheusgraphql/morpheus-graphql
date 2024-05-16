{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf
  ( setup,
    Env (..),
  )
where

import Data.Text
import HConf.ConfigT (info)
import HConf.Env (Env (..))
import HConf.Hie (genHie)
import HConf.Package (checkPackages)
import HConf.Stack (setupStack)
import HConf.Version (parseVersion)
import HConf.Yaml (open, save)
import Prelude

setup :: String -> Env -> IO ()
setup ver = open $ do
  info "setup:start"
  parseVersion (pack ver) >>= setupStack
  genHie
  checkPackages
  info "setup:end"
  save
