{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf
  ( Config (..),
    updateStack,
    genHie,
    Stack,
    writeYaml,
    readYaml,
    checkPackages,
  )
where

import HConf.Config (Config (..))
import HConf.Hie (genHie)
import HConf.Package (checkPackages)
import HConf.Stack (Stack, updateStack)
import HConf.Yaml (readYaml, writeYaml)
