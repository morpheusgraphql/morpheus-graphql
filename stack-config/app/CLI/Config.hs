{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI.Config
  ( Config (..),
    readConfig,
    writeConfig,
  )
where

import Config (Config (..), parseYaml, serializeYaml)
import qualified Data.ByteString as L
  ( readFile,
    writeFile,
  )
import Data.Yaml (decodeThrow)
import Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare, setConfDropNull)
import Relude

readConfig :: FilePath -> IO Config
readConfig = L.readFile >=> parseYaml

writeConfig :: FilePath -> Config -> IO ()
writeConfig path = L.writeFile path . serializeYaml
