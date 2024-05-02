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

import Config (Config (..), compareFields)
import qualified Data.ByteString as L
  ( readFile,
    writeFile,
  )
import Data.Yaml (decodeThrow)
import Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare, setConfDropNull)
import Relude

readConfig :: FilePath -> IO Config
readConfig path = do
  file <- L.readFile path
  decodeThrow file

writeConfig :: FilePath -> Config -> IO ()
writeConfig path config = do
  let file = encodePretty (setConfDropNull True $ setConfCompare compareFields defConfig) config
  L.writeFile path file
