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
import HConf.Log (info, label, task)
import HConf.Package (checkPackages)
import HConf.Stack (setupStack)
import HConf.Version (parseVersion)
import HConf.Yaml (open, save)
import Prelude

setup :: String -> Env -> IO ()
setup ver =
  open $
    label "setup" $ do
      parseVersion (pack ver) >>= setupStack
      genHie
      checkPackages
      label "hconf" $ task "hconf.yaml" save
      info "\nok"
