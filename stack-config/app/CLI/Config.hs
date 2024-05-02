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

import Config (Config (..))
import qualified Data.ByteString as L
  ( readFile,
    writeFile,
  )
import Data.Yaml
  ( decodeThrow,
    encode,
  )
import Relude

readConfig :: FilePath -> IO Config
readConfig path = do
  file <- L.readFile path
  decodeThrow file

writeConfig :: FilePath -> Config -> IO ()
writeConfig path config = do
  let file = encode config
  L.writeFile path file
