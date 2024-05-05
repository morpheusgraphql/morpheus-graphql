{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config
  ( Config (..),
    PkgGroup (..),
    updateStack,
    genHie,
    Stack,
    writeYaml,
    readYaml,
  )
where

import Config.File (readYaml, writeYaml)
import Config.Hie
import Config.Stack
import Config.Types

-- export const setup = async (version: string) => {
--   checkPackages();
-- };