{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf
  ( setup,
    Env (..),
  )
where

import Data.Text
import HConf.Env (Env (..))
import HConf.Hie (genHie)
import HConf.Log (info, label, listItem)
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
  label "hconf" $ do
    listItem ("hconf.yaml" :: String)
    save
  info "\nsetup:end"
